source(here::here("analysis", "00_setup.R"))

Hospital_FP10_data <- read_excel(here("data", "secondary_care_fp10", "FP10_data.xlsx"))
Hospital_FP10_data <- Hospital_FP10_data %>%
  mutate(PERIOD = as.Date(paste0(PERIOD, "01"), format = "%Y%m%d")) %>%
  mutate(
    BNF_CODE            = coalesce(BNF_CODE, `BNF CODE`),
    BNF_NAME            = coalesce(BNF_NAME, `BNF NAME`),
    HOSPITAL_TRUST_CODE = coalesce(HOSPITAL_TRUST_CODE, `HOSPITAL TRUST CODE`),
    HOSPITAL_TRUST      = coalesce(HOSPITAL_TRUST, `HOSPITAL TRUST`),
    TOTAL_QUANTITY      = coalesce(TOTAL_QUANTITY, `TOTAL QUANTITY`),
    TOTAL_ITEMS         = coalesce(TOTAL_ITEMS, `TOTAL ITEMS`),
    TOTAL_ACTUAL_COST   = coalesce(TOTAL_ACTUAL_COST, `TOTAL ACTUAL COST`),
    TOTAL_NIC           = coalesce(TOTAL_NIC, `TOTAL NIC`)
  ) %>%
  select(-`BNF CODE`, -`BNF NAME`, -`HOSPITAL TRUST CODE`, -`HOSPITAL TRUST`,
         -`TOTAL QUANTITY`, -`TOTAL ITEMS`, -`TOTAL ACTUAL COST`, -`TOTAL NIC`)

periods_to_remove <- as.Date(paste0(c(
  "202401", "202402", "202403", "202404",
  "202405", "202406", "202407", "202408",
  "202409", "202410", "202411", "202412"
), "01"), format = "%Y%m%d")

Hospital_FP10_data <- Hospital_FP10_data %>%
  filter(!(PERIOD %in% periods_to_remove), !is.na(PERIOD))

Hospital_FP10_data2024 <- read_excel(here("data", "secondary_care_fp10", "FP10_2024.xlsx")) %>%
  rename(
    BNF_CODE = `BNF CODE`,
    BNF_NAME = `BNF NAME`,
    HOSPITAL_TRUST_CODE = `HOSPITAL TRUST CODE`,
    HOSPITAL_TRUST = `HOSPITAL TRUST`,
    TOTAL_QUANTITY = `TOTAL QUANTITY`,
    TOTAL_ITEMS = `TOTAL ITEMS`,
    TOTAL_ACTUAL_COST = `TOTAL ACTUAL COST`,
    TOTAL_NIC = `TOTAL NIC`
  ) %>%
  mutate(
    PERIOD = as.Date(paste0(as.character(PERIOD), "01"), format = "%Y%m%d"),
    TOTAL_ACTUAL_COST = as.numeric(TOTAL_ACTUAL_COST)
  )

Hospital_FP10_all <- bind_rows(Hospital_FP10_data, Hospital_FP10_data2024)

New_Hospital_FP10_data <- Hospital_FP10_all %>%
  mutate(
    chemical = case_when(
      BNF_NAME %in% c(
        "Lithium carbonate 400mg modified-release tablets",
        "Lithium carbonate 200mg modified-release tablets",
        "Priadel 200mg modified-release tablets",
        "Priadel 400mg modified-release tablets",
        "Lithium carbonate 450mg modified-release tablets",
        "Priadel 520mg/5ml liquid",
        "Lithium carbonate 250mg tablets",
        "Camcolit 400 modified-release tablets",
        "Priadel_Tab 400mg",
        "Lithium Carb_Tab 400mg M/R",
        "Lithium Carb_Tab 200mg M/R",
        "Priadel_Tab 200mg",
        "Camcolit 400_Tab 400mg",
        "Lithium Carb_Tab 250mg",
        "Camcolit 250_Tab 250mg",
        "Lithium Carb_Tab 450mg M/R",
        "Lithium Carb_Liq Spec 200mg/5ml",
        "Lithium carbonate 200mg/5ml oral suspension",
        "Camcolit 250 tablets",
        "Liskonum 450mg modified-release tablets",
        "Liskonum_Tab 450mg M/R"
      ) ~ "Lithium Carbonate",
      BNF_NAME %in% c(
        "Li-Liquid 509mg/5ml oral solution",
        "Lithium citrate 509mg/5ml oral solution",
        "Lithium citrate 520mg/5ml oral solution sugar free",
        "Lithium Cit_Oral Soln 1.018g/5ml",
        "Lithium Cit_Oral Soln 520mg/5ml S/F",
        "Lithium Cit_Oral Soln 509mg/5ml",
        "Li-Liquid_Syr 10.8mmol/5ml",
        "Priadel_Liq 520mg/5ml S/F",
        "Li-Liquid_Syr 5.4mmol/5ml",
        "Lithium citrate 1.018g/5ml oral solution",
        "Li-Liquid 1.018g/5ml oral solution"
      ) ~ "Lithium Citrate",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(
    chemical = case_when(
      str_detect(chemical, regex("Lithium Carbonate", ignore_case = TRUE)) ~ "Lithium carbonate",
      str_detect(chemical, regex("Lithium Citrate", ignore_case = TRUE)) ~ "Lithium citrate",
      TRUE ~ "Other"
    ),
    quantity_basis = case_when(
      str_detect(BNF_NAME, regex("liq|oral soln|solution|syrup|ml", ignore_case = TRUE)) ~ "ml",
      str_detect(BNF_NAME, regex("tab|tablet", ignore_case = TRUE)) ~ "tablet",
      TRUE ~ NA_character_
    ),
    strength_numerator_value = case_when(
      str_detect(BNF_NAME, "mg/5ml") ~ as.numeric(str_extract(BNF_NAME, "(?i)(\\d+\\.?\\d*)(?=mg/5ml)")),
      str_detect(BNF_NAME, "mg") & !str_detect(BNF_NAME, "mg/5ml") ~ as.numeric(str_extract(BNF_NAME, "(?i)(\\d+\\.?\\d*)(?=mg)")),
      str_detect(BNF_NAME, "g/5ml") ~ as.numeric(str_extract(BNF_NAME, "(?i)(\\d+\\.?\\d*)(?=g/5ml)")) * 1000,
      TRUE ~ NA_real_
    ),
    total_mg = case_when(
      chemical == "Lithium carbonate" & quantity_basis == "tablet" ~ TOTAL_QUANTITY * strength_numerator_value,
      TRUE ~ NA_real_
    ),
    mmol = case_when(
      chemical == "Lithium carbonate" ~ total_mg / 37.04,
      chemical == "Lithium citrate" & str_detect(BNF_NAME, "509") ~ (TOTAL_QUANTITY / 5) * 5.4,
      chemical == "Lithium citrate" & str_detect(BNF_NAME, "520") ~ (TOTAL_QUANTITY / 5) * 5.4,
      chemical == "Lithium citrate" & str_detect(BNF_NAME, "1.018") ~ (TOTAL_QUANTITY / 5) * 10.8,
      TRUE ~ NA_real_
    ),
    DDD = mmol / 24
  )

secondary_care_trusts <- read_csv(here("data", "secondary_care", "secondary_care_trusts.csv"), show_col_types = FALSE)
trust_mapping <- secondary_care_trusts %>%
  mutate(trust_code_prefix = substr(`Trust Code`, 1, 3)) %>%
  select(trust_code_prefix, region = Region) %>%
  distinct(trust_code_prefix, .keep_all = TRUE)

region_mapping <- c(
  "Y60" = "Midlands",
  "Y63" = "North East And Yorkshire",
  "Y59" = "South East",
  "Y61" = "East of England",
  "Y62" = "North West",
  "Y56" = "London",
  "Y58" = "South West",
  "RCE" = "North East And Yorkshire",
  "RK7" = "North East And Yorkshire",
  "RQ4" = "Midlands",
  "RRQ" = "London",
  "RNJ" = "London",
  "RMK" = "North West",
  "RNH" = "London",
  "G6V" = "London"
)

New_Hospital_FP10_data <- New_Hospital_FP10_data %>%
  mutate(trust_code_prefix = substr(HOSPITAL_TRUST_CODE, 1, 3)) %>%
  left_join(trust_mapping, by = "trust_code_prefix") %>%
  filter(trust_code_prefix != "Y99") %>%
  mutate(region = if_else(
    is.na(region) & trust_code_prefix %in% names(region_mapping),
    region_mapping[trust_code_prefix],
    region
  )) %>%
  filter(year(PERIOD) <= 2024L)

Hospital_FP10_data <- New_Hospital_FP10_data %>%
  mutate(PERIOD = format(as.Date(PERIOD), "%Y"))

HospitalFP10_DDD_by_year <- Hospital_FP10_data %>%
  group_by(PERIOD) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

hospital_fp10_product_DDD <- New_Hospital_FP10_data %>%
  group_by(product_code = BNF_CODE, product_name = BNF_NAME) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_DDD), product_name)

hospitalFP10_line <- ggplot(HospitalFP10_DDD_by_year,
                            aes(x = as.integer(PERIOD), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = colour_care_fp10) +
  geom_point(size = 3, color = colour_care_fp10) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = HospitalFP10_DDD_by_year$total_DDD / 1e6,
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(breaks = 2017:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave(here(plots_dir, "hospital_fp10_line_trends.png"), hospitalFP10_line, width = 8, height = 5, dpi = 300)

HospitalFP10_DDD_by_year_region <- Hospital_FP10_data %>%
  mutate(year = as.integer(PERIOD)) %>%
  group_by(year, region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  left_join(population_df %>% select(region, population), by = "region") %>%
  mutate(DDDs_per_1000 = round(total_DDD / population * 1000, 2))

Hospital_FP10_total_DDD_by_region_2024 <- New_Hospital_FP10_data %>%
  mutate(PERIOD_date = as.Date(PERIOD)) %>%
  filter(year(PERIOD_date) == 2024) %>%
  group_by(region) %>%
  summarise(total_DDD_2024 = sum(DDD, na.rm = TRUE)) %>%
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
  mutate(DDDs_per_1000 = round(total_DDD_2024 / population * 1000, 2))

coverage_data_fp10 <- nhs_regions_sf %>%
  left_join(Hospital_FP10_total_DDD_by_region_2024, by = "region")

fp10_label_d <- coverage_map_label_layers_data(coverage_data_fp10, "region")
fp10_label_pts <- dplyr::bind_rows(fp10_label_d$other, fp10_label_d$london_txt)
fp10_label_halo <- coverage_map_label_halo_rect(fp10_label_pts)

FP10_coverage_plot <- ggplot() +
  geom_sf(data = coverage_data_fp10, aes(fill = DDDs_per_1000), colour = "black", linewidth = 0.8) +
  geom_segment(
    data = fp10_label_d$london_seg,
    aes(x = lon, y = lat, xend = lon_end, yend = lat_end),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = coverage_map_leader_linewidth,
    lineend = "round"
  ) +
  geom_polygon(
    data = fp10_label_halo,
    aes(x = lon, y = lat, group = group),
    inherit.aes = FALSE,
    fill = "white",
    colour = "grey25",
    linewidth = 0.35
  ) +
  geom_text(
    data = fp10_label_pts,
    aes(x = lon, y = lat, label = label),
    inherit.aes = FALSE,
    colour = "black",
    fontface = "bold",
    size = coverage_map_value_label_size
  ) +
  scale_fill_gradientn(
    colors = colour_care_fp10_map,
    breaks = function(lims) c(lims[1], lims[2]),
    labels = coverage_map_colourbar_break_labels,
    na.value = "grey90",
    guide = guide_colourbar(
      title = "DDDs per 1,000 population",
      title.position = "top",
      barheight = unit(3.2, "cm"),
      barwidth = unit(0.55, "cm"),
      ticks = FALSE,
      reverse = TRUE,
      frame.colour = "black",
      frame.linewidth = 0.35
    )
  ) +
  theme_lithium() +
  theme(
    legend.position = coverage_map_legend_position,
    legend.text = element_text(size = coverage_map_legend_text_size),
    legend.title = element_text(size = coverage_map_legend_title_size),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(5.5, coverage_map_plot_margin_right, 5.5, 5.5)
  ) +
  coord_sf(datum = NA, clip = "off") +
  xlab("") +
  ylab("")
ggsave(here(plots_dir, "fp10_coverage_map.png"), FP10_coverage_plot, width = 8, height = 6, dpi = 300)

FP10hist <- ggplot(Hospital_FP10_total_DDD_by_region_2024, aes(x = region, y = DDDs_per_1000)) +
  geom_col(fill = colour_care_fp10, color = colour_care_fp10) +
  geom_text(aes(label = sprintf("%.2f", DDDs_per_1000)), vjust = -0.3, size = 3.5) +
  theme_lithium() +
  xlab("Region") +
  ylab("DDDs per 1,000 population") +
  scale_y_to_next_tick(
    values = Hospital_FP10_total_DDD_by_region_2024$DDDs_per_1000,
    labels = scales::number_format(accuracy = 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = axis_tick_label_size),
    axis.text.y = element_text(size = axis_tick_label_size),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave(here(plots_dir, "fp10_hist_ddd_pop.png"), FP10hist, width = 8, height = 5, dpi = 300)

write.csv(
  format_ddd_by_year_for_export(HospitalFP10_DDD_by_year, "PERIOD"),
  here(data_dir, "hospital_fp10_DDD_by_year.csv"),
  row.names = FALSE
)
write.csv(hospital_fp10_product_DDD, here(data_dir, "hospital_fp10_product_DDD.csv"), row.names = FALSE)
write.csv(Hospital_FP10_total_DDD_by_region_2024, here(data_dir, "hospital_fp10_DDD_by_region_2024.csv"), row.names = FALSE)
write.csv(HospitalFP10_DDD_by_year_region, here(data_dir, "hospital_fp10_DDD_by_year_region.csv"), row.names = FALSE)
message("FP10 analysis complete. Outputs saved to ", output_dir)
