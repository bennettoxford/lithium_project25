source(here::here("analysis", "00_setup.R"))

# Import primary care dataset
PrimaryCare_Lithium <- read.csv(gzfile(here("data", "primary_care", "primary_lithium.csv.gz")))
PrimaryCare_Lithium <- PrimaryCare_Lithium %>%
  mutate(month = as.Date(month))

Practice_codes <- read_excel(here("data", "primary_care", "practice_codes.xlsx"))
merged_data <- PrimaryCare_Lithium %>%
  left_join(Practice_codes, by = c("practice" = "code")) %>%
  filter(setting == 4)

df_primarycare2 <- read.csv(here("data", "primary_care", "primary_care.csv"))
PRIMARYCARE_dataset <- merge(
  merged_data,
  df_primarycare2[, c("bnf_code", "strnt_nmrtr_val")],
  by = "bnf_code",
  all.x = TRUE
)

PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(
    chemical = case_when(
      bnf_name %in% c(
        "Camcolit 250 tablets",
        "Camcolit 400 modified-release tablets",
        "Liskonum 450mg modified-release tablets",
        "Lithium carbonate 200mg modified-release tablets",
        "Lithium carbonate 200mg/5ml oral suspension",
        "Lithium carbonate 250mg tablets",
        "Lithium carbonate 400mg modified-release tablets",
        "Lithium carbonate 450mg modified-release tablets",
        "Lithonate 400mg modified-release tablets",
        "Priadel 200mg modified-release tablets",
        "Priadel 400mg modified-release tablets"
      ) ~ "Lithium Carbonate",
      bnf_name %in% c(
        "Li-Liquid 1.018g/5ml oral solution",
        "Li-Liquid 509mg/5ml oral solution",
        "Lithium citrate 1.018g/5ml oral solution",
        "Lithium citrate 509mg/5ml oral solution",
        "Lithium citrate 520mg/5ml oral solution sugar free",
        "Priadel 520mg/5ml liquid",
        "Lithium citrate 10.8mmol/5ml oral solution sugar free"
      ) ~ "Lithium Citrate",
      TRUE ~ "Other"
    ),
    quantity_mg = quantity * strnt_nmrtr_val,
    mmol = case_when(
      chemical == "Lithium Carbonate" ~ quantity_mg / 37.04,
      chemical == "Lithium Citrate" ~ quantity_mg / 69.98,
      TRUE ~ NA_real_
    ),
    DDD = mmol / 24
  )

region_mapping <- c(
  "Y60" = "Midlands",
  "Y63" = "North East And Yorkshire",
  "Y59" = "South East",
  "Y61" = "East of England",
  "Y62" = "North West",
  "Y56" = "London",
  "Y58" = "South West"
)
PRIMARYCARE_dataset$Region <- region_mapping[PRIMARYCARE_dataset$regional_team]
PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  filter(month >= as.Date("2015-01-01") & month <= as.Date("2024-12-31"))

PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(year = format(as.Date(month), "%Y"))

Primaryy_DDD_by_year <- PRIMARYCARE_dataset %>%
  filter(year != "2025") %>%
  group_by(year) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

primary_line <- ggplot(Primaryy_DDD_by_year, aes(x = as.integer(year), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = "orange") +
  geom_point(size = 3, color = "blue") +
  labs(
    title = "Primary Care: Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2015–2024)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    limits = c(0, 14),
    expand = c(0, 0),
    labels = function(x) format(x, scientific = FALSE, big.mark = ",")
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave(here(plots_dir, "primary_line_trends.png"), primary_line, width = 8, height = 5, dpi = 300)

primary_bar <- ggplot(Primaryy_DDD_by_year, aes(x = as.factor(year), y = total_DDD / 1e6)) +
  geom_bar(stat = "identity", fill = "orange", width = 0.6) +
  geom_text(
    aes(label = format(round(total_DDD / 1e6, 1), nsmall = 1)),
    vjust = -0.5,
    size = 4.2,
    fontface = "bold"
  ) +
  labs(
    title = "Primary Care: Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2015–2024)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    labels = function(x) format(x, scientific = FALSE, big.mark = ","),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )
ggsave(here(plots_dir, "primary_bar_trends.png"), primary_bar, width = 8, height = 5, dpi = 300)

lithium_df_primary <- PRIMARYCARE_dataset %>%
  group_by(Region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  mutate(Region = as.factor(Region)) %>%
  filter(!is.na(Region))

total_primary_DDD_by_region_2024 <- PRIMARYCARE_dataset %>%
  filter(year(month) == 2024) %>%
  group_by(Region) %>%
  summarise(total_DDD_2024 = sum(DDD, na.rm = TRUE))

primary_lithium_df <- lithium_df_primary %>%
  left_join(total_primary_DDD_by_region_2024, by = "Region") %>%
  left_join(population_df %>% select(Region, population), by = "Region") %>%
  mutate(`DDD/population` = total_DDD_2024 / population)

coverage_data_primary <- nhs_regions_sf %>%
  left_join(primary_lithium_df, by = "Region")
unique_values <- sort(unique(coverage_data_primary$`DDD/population`))
breaks <- c(0, unique_values, max(unique_values, na.rm = TRUE) + 10)
labels <- scales::label_number(breaks)

primary_coverage_plot <- coverage_data_primary %>%
  ggplot() +
  geom_sf(aes(fill = `DDD/population`), colour = "black", linewidth = 0.8) +
  geom_sf_text(aes(label = Region), colour = "white", size = 3) +
  scale_fill_gradientn(
    colors = c("#ffd13a", "#ff7c00", "#f20c51"),
    breaks = breaks,
    labels = labels,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.5),
    legend.text = element_text(hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(title = "Lithium (DDD)/ population")) +
  coord_sf(datum = NA) +
  xlab("") +
  ylab("")
ggsave(here(plots_dir, "primary_coverage_map.png"), primary_coverage_plot, width = 8, height = 6, dpi = 300)

primaryhist <- ggplot(primary_lithium_df, aes(x = Region, y = `DDD/population`)) +
  geom_col(fill = "#1f77b4", color = "#6baed6") +
  geom_text(aes(label = sprintf("%.3f", `DDD/population`)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (Total DDD for 2024) / population") +
  labs(
    title = "Regional Lithium Use in Primary Care",
    subtitle = "Average DDDs per Person (2024) (prescription)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave(here(plots_dir, "primary_hist_ddd_pop.png"), primaryhist, width = 8, height = 5, dpi = 300)

Primary_DDD_by_year_region <- PRIMARYCARE_dataset %>%
  mutate(month = as.Date(month)) %>%
  mutate(year = year(month)) %>%
  filter(year != 2025) %>%
  group_by(year, Region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(population_df %>% select(Region, population), by = "Region") %>%
  mutate(DDD_population = total_DDD / population)

write.csv(Primaryy_DDD_by_year, here(data_dir, "primary_DDD_by_year.csv"), row.names = FALSE)
write.csv(primary_lithium_df, here(data_dir, "primary_lithium_by_region.csv"), row.names = FALSE)
write.csv(Primary_DDD_by_year_region, here(data_dir, "primary_DDD_by_year_region.csv"), row.names = FALSE)
message("Primary analysis complete. Outputs saved to ", output_dir)
