source(here::here("analysis", "00_setup.R"))

# Import FP10 data from monthly CSV files (data/secondary_care_fp10/fp10_YYYYMM.csv)
fp10_files <- list.files(
  here("data", "secondary_care_fp10"),
  pattern = "^fp10_[0-9]{6}\\.csv$",
  full.names = TRUE
)
if (length(fp10_files) == 0) {
  stop("No fp10_YYYYMM.csv files found in data/secondary_care_fp10/")
}
Hospital_FP10_all <- fp10_files %>%
  lapply(read.csv) %>%
  bind_rows() %>%
  mutate(
    PERIOD = as.Date(paste0(as.character(PERIOD), "01"), format = "%Y%m%d"),
    TOTAL_ACTUAL_COST = as.numeric(TOTAL_ACTUAL_COST)
  ) %>%
  filter(!is.na(PERIOD))

# Chemical and strength lookup from lithium_products (same as primary care)
lithium_products <- read.csv(here("data", "lithium_products.csv"))
New_Hospital_FP10_data <- Hospital_FP10_all %>%
  left_join(
    lithium_products %>% select(bnf_code, strnt_nmrtr_val, chemical),
    by = c("BNF_CODE" = "bnf_code")
  ) %>%
  mutate(
    chemical = coalesce(chemical, "Other"),
    quantity_mg = TOTAL_QUANTITY * strnt_nmrtr_val,
    mmol = case_when(
      chemical == "Lithium carbonate" ~ quantity_mg / 37.04,
      chemical == "Lithium citrate" ~ quantity_mg / 69.98,
      TRUE ~ NA_real_
    ),
    DDD = mmol / 24
  )

# Trust-to-region mapping from ord_trusts; resolve via successors when region is NA
ord_trusts <- read_csv(here("data", "ord_trusts.csv"), show_col_types = FALSE)

resolve_region <- function(code, visited = character()) {
  if (code %in% visited) return(NA_character_)  # avoid cycles
  row <- ord_trusts %>% filter(ods_code == code)
  if (nrow(row) == 0) return(NA_character_)
  row <- row[1, ]
  if (!is.na(row$region) && row$region != "") return(row$region)
  # Try successors (orgs that replaced this one)
  if (!is.na(row$successors) && row$successors != "") {
    succ <- trimws(strsplit(as.character(row$successors), ",")[[1]])[1]
    out <- resolve_region(succ, c(visited, code))
    if (!is.na(out)) return(out)
  }
  # Try predecessors of other orgs (orgs that list this as predecessor = our successors)
  pred_rows <- ord_trusts %>%
    filter(!is.na(predecessors)) %>%
    filter(purrr::map_lgl(predecessors, ~ code %in% trimws(strsplit(as.character(.x), ",")[[1]])))
  if (nrow(pred_rows) > 0) {
    out <- resolve_region(pred_rows$ods_code[1], c(visited, code))
    if (!is.na(out)) return(out)
  }
  NA_character_
}

trust_mapping <- ord_trusts %>%
  mutate(region_resolved = purrr::map_chr(ods_code, resolve_region)) %>%
  filter(!is.na(region_resolved)) %>%
  mutate(trust_code_prefix = substr(ods_code, 1, 3)) %>%
  select(trust_code_prefix, region = region_resolved) %>%
  distinct(trust_code_prefix, .keep_all = TRUE)

New_Hospital_FP10_data <- New_Hospital_FP10_data %>%
  mutate(trust_code_prefix = substr(HOSPITAL_TRUST_CODE, 1, 3)) %>%
  left_join(trust_mapping, by = "trust_code_prefix") %>%
  filter(trust_code_prefix != "Y99")

unmapped <- New_Hospital_FP10_data %>% filter(is.na(region)) %>% distinct(trust_code_prefix, HOSPITAL_TRUST_CODE, HOSPITAL_TRUST)
if (nrow(unmapped) > 0) {
  stop(
    "Could not resolve region for ", nrow(unmapped), " trust(s). ",
    "Add to ord_trusts or fix successor/predecessor chain. Examples: ",
    paste(head(unmapped$trust_code_prefix, 5), collapse = ", ")
  )
}

Hospital_FP10_data <- New_Hospital_FP10_data %>%
  mutate(PERIOD = format(as.Date(PERIOD), "%Y"))

HospitalFP10_DDD_by_year <- Hospital_FP10_data %>%
  group_by(PERIOD) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop")

max_value <- max(HospitalFP10_DDD_by_year$total_DDD) / 1e6
hospitalFP10_line <- ggplot(HospitalFP10_DDD_by_year,
                            aes(x = as.integer(PERIOD), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = "#2E8B57") +
  geom_point(size = 3, color = "#FFD700") +
  labs(
    title = "Secondary care (Hospital FP10): Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2017–2025)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    limits = c(0, max_value * 1.1),
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(breaks = 2017:2025) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave(here(plots_dir, "hospital_fp10_line_trends.png"), hospitalFP10_line, width = 8, height = 5, dpi = 300)

HospitalFP10_DDD_by_year_region <- Hospital_FP10_data %>%
  mutate(year = as.integer(PERIOD)) %>%
  group_by(year, region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  left_join(population_df %>% select(region, population), by = "region") %>%
  mutate(DDD_population = total_DDD / population)

Hospital_FP10_total_DDD_by_region_2025 <- New_Hospital_FP10_data %>%
  mutate(PERIOD_date = as.Date(PERIOD)) %>%
  filter(year(PERIOD_date) == 2025) %>%
  group_by(region) %>%
  summarise(total_DDD_2025 = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    name = case_when(
      region == "East Of England" ~ "UKH",
      region == "North West" ~ "UKD",
      region == "North East And Yorkshire" ~ "UKE",
      region == "London" ~ "UKF",
      region == "Midlands" ~ "UKG",
      region == "South East" ~ "UKJ",
      region == "South West" ~ "UKK",
      TRUE ~ NA_character_
    ),
    region = as.factor(region)
  ) %>%
  filter(!is.na(region)) %>%
  left_join(population_df %>% select(region, population), by = "region") %>%
  mutate(`DDD/population` = total_DDD_2025 / population)

coverage_data_fp10 <- nhs_regions_sf %>%
  left_join(Hospital_FP10_total_DDD_by_region_2025, by = "region")
unique_values <- sort(unique(coverage_data_fp10$`DDD/population`))
breaks <- c(0, unique_values, max(unique_values, na.rm = TRUE) + 10)

FP10_coverage_plot <- coverage_data_fp10 %>%
  st_transform(27700) %>%
  ggplot() +
  geom_sf(aes(fill = `DDD/population`), colour = "black", linewidth = 0.8) +
  geom_sf_text(aes(label = region), colour = "white", size = 3) +
  scale_fill_gradientn(
    colors = c("#ffd13a", "#ff7c00", "#f20c51"),
    breaks = breaks,
    labels = scales::label_number(accuracy = 0.001),
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.5),
    legend.text = element_text(hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold", size = 16)
  ) +
  guides(fill = guide_legend(title = "Lithium (DDD)/ population")) +
  coord_sf(crs = 27700, datum = NA) +
  labs(
    title = "Hospital FP10 data",
    subtitle = "Total Daily Defined Dose of Lithium in 2025 regionally, per population estimates"
  ) +
  xlab("") +
  ylab("")
ggsave(here(plots_dir, "fp10_coverage_map.png"), FP10_coverage_plot, width = 8, height = 6, dpi = 300)

max_y <- max(Hospital_FP10_total_DDD_by_region_2025$`DDD/population`, na.rm = TRUE)
buffer <- max_y * 0.1
FP10hist <- ggplot(Hospital_FP10_total_DDD_by_region_2025, aes(x = region, y = `DDD/population`)) +
  geom_col(fill = "#FFD700", color = "#FFD700") +
  geom_text(aes(label = round(`DDD/population`, 3)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (Total DDD for 2025) / population") +
  labs(
    title = "Regional Lithium Use in Secondary Care, FP10 hopsital data",
    subtitle = "Average DDDs per Person (2025)"
  ) +
  scale_y_continuous(
    limits = c(0, max_y + buffer),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave(here(plots_dir, "fp10_hist_ddd_pop.png"), FP10hist, width = 8, height = 5, dpi = 300)

write.csv(HospitalFP10_DDD_by_year, here(data_dir, "hospital_fp10_DDD_by_year.csv"), row.names = FALSE)
write.csv(Hospital_FP10_total_DDD_by_region_2025, here(data_dir, "hospital_fp10_DDD_by_region_2025.csv"), row.names = FALSE)
write.csv(HospitalFP10_DDD_by_year_region, here(data_dir, "hospital_fp10_DDD_by_year_region.csv"), row.names = FALSE)
message("FP10 analysis complete. Outputs saved to ", output_dir)
