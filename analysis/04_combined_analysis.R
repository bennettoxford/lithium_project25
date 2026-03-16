source(here::here("analysis", "00_setup.R"))

Primaryy_DDD_by_year <- read.csv(here(data_dir, "primary_DDD_by_year.csv"))
Secondary_DDD_by_year <- read.csv(here(data_dir, "secondary_DDD_by_year.csv"))
HospitalFP10_DDD_by_year <- read.csv(here(data_dir, "hospital_fp10_DDD_by_year.csv"))
primary_lithium_df <- read.csv(here(data_dir, "primary_lithium_by_region.csv"))
secondary_lithium_df <- read.csv(here(data_dir, "secondary_lithium_by_region.csv"))
Hospital_FP10_total_DDD_by_region_2024 <- read.csv(here(data_dir, "hospital_fp10_DDD_by_region_2024.csv"))
Primary_DDD_by_year_region <- read.csv(here(data_dir, "primary_DDD_by_year_region.csv"))
Secondary_DDD_by_year_region <- read.csv(here(data_dir, "secondary_DDD_by_year_region.csv"))
HospitalFP10_DDD_by_year_region <- read.csv(here(data_dir, "hospital_fp10_DDD_by_year_region.csv"))

# Combined primary + secondary trends
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

secondary_line <- ggplot(Secondary_DDD_by_year, aes(x = as.integer(year), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = "#00BFC4") +
  geom_point(size = 3, color = "#F8766D") +
  labs(
    title = "Secondary Care: Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2019–2024)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    limits = c(0, 1.2),
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(breaks = 2019:2024) +
  coord_cartesian(ylim = c(0, 1.2)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

combined_plot <- primary_line / secondary_line
ggsave(here(plots_dir, "combined_primary_secondary_trends.png"), combined_plot, width = 8, height = 10, dpi = 300)

# Combined line plot (all three sources)
all_years <- c(
  as.numeric(Primaryy_DDD_by_year$year),
  as.numeric(Secondary_DDD_by_year$year),
  as.numeric(HospitalFP10_DDD_by_year$PERIOD)
)

combined_line_plot <- ggplot() +
  geom_line(data = Primaryy_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6),
            color = "orange", linewidth = 1.2) +
  geom_point(data = Primaryy_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6),
             color = "blue", size = 3) +
  geom_line(data = Secondary_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6),
            color = "#00BFC4", linewidth = 1.2) +
  geom_point(data = Secondary_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6),
             color = "#F8766D", size = 3) +
  geom_line(data = HospitalFP10_DDD_by_year,
            aes(x = as.integer(PERIOD), y = total_DDD / 1e6),
            color = "#2E8B57", linewidth = 1.2) +
  geom_point(data = HospitalFP10_DDD_by_year,
             aes(x = as.integer(PERIOD), y = total_DDD / 1e6),
             color = "#FFD700", size = 3) +
  labs(
    title = "Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    limits = c(0, ceiling(max(
      max(Primaryy_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(Secondary_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(HospitalFP10_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      1.2
    ) * 1.1)),
    breaks = seq(0, ceiling(max(
      max(Primaryy_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(Secondary_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(HospitalFP10_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      1.2
    ) * 1.1), by = 1),
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    limits = c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )
ggsave(here(plots_dir, "combined_line_all_sources.png"), combined_line_plot, width = 10, height = 6, dpi = 300)

combined_line_plot_legend <- ggplot() +
  geom_line(data = Primaryy_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6, color = "Primary Care"),
            linewidth = 1.2) +
  geom_point(data = Primaryy_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6),
             color = "blue", size = 3) +
  geom_line(data = Secondary_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6, color = "Secondary Care"),
            linewidth = 1.2) +
  geom_point(data = Secondary_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6),
             color = "#F8766D", size = 3) +
  geom_line(data = HospitalFP10_DDD_by_year,
            aes(x = as.integer(PERIOD), y = total_DDD / 1e6, color = "Hospital FP10 Care"),
            linewidth = 1.2) +
  geom_point(data = HospitalFP10_DDD_by_year,
             aes(x = as.integer(PERIOD), y = total_DDD / 1e6),
             color = "#FFD700", size = 3) +
  scale_color_manual(
    name = "Care Type",
    values = c(
      "Primary Care" = "orange",
      "Secondary Care" = "#00BFC4",
      "Hospital FP10 Care" = "#2E8B57"
    )
  ) +
  labs(
    title = "Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    limits = c(0, ceiling(max(
      max(Primaryy_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(Secondary_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(HospitalFP10_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      1.2
    ) * 1.1)),
    breaks = seq(0, ceiling(max(
      max(Primaryy_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(Secondary_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(HospitalFP10_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      1.2
    ) * 1.1), by = 1),
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    limits = c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(face = "plain"),
    axis.title.y = element_text(face = "plain"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.background = element_rect(color = "black", linewidth = 0.5),
    legend.key = element_rect(fill = "white", color = NA)
  )
ggsave(here(plots_dir, "combined_line_all_sources_legend.png"), combined_line_plot_legend, width = 10, height = 6, dpi = 300)

# Stacked bar plot
primary_lithium_df <- primary_lithium_df %>%
  mutate(
    Region = ifelse(tolower(Region) == "east of england", "East of England", as.character(Region)),
    Source = "Primary"
  )
secondary_lithium_df <- secondary_lithium_df %>%
  mutate(
    Region = ifelse(tolower(region) == "east of england", "East of England", as.character(region)),
    Source = "Secondary"
  )
Hospital_FP10_total_DDD_by_region_2024 <- Hospital_FP10_total_DDD_by_region_2024 %>%
  mutate(
    Region = ifelse(tolower(region) == "east of england", "East of England", as.character(region)),
    Source = "Hospital FP10"
  ) %>%
  select(-region)

combined_df_all <- bind_rows(primary_lithium_df, secondary_lithium_df, Hospital_FP10_total_DDD_by_region_2024) %>%
  mutate(Source = factor(Source, levels = c("Primary", "Secondary", "Hospital FP10")))

stacked_bar_plot <- ggplot(combined_df_all, aes(x = Region, y = `DDD.population`, fill = Source)) +
  geom_col(color = "black") +
  scale_fill_manual(
    values = c("Primary" = "orange", "Secondary" = "#00BFC4", "Hospital FP10" = "#2E8B57")
  ) +
  theme_minimal() +
  labs(
    title = "Regional Lithium Use by Care Level",
    subtitle = "Primary, Secondary, Hospital (FP10) in 2024",
    x = "Region",
    y = "Lithium usage (DDD/population)",
    fill = "Care Level"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 12, face = "bold")
  )
ggsave(here(plots_dir, "stacked_bar_regional_by_care.png"), stacked_bar_plot, width = 10, height = 6, dpi = 300)

# National DDD trends
standardise_region <- function(region) {
  region <- tolower(region)
  region <- trimws(region)
  region <- gsub(" of ", " Of ", region)
  region <- tools::toTitleCase(region)
  return(region)
}

Primary_clean <- Primary_DDD_by_year_region %>%
  mutate(
    year = as.integer(year),
    region = if ("Region" %in% names(.)) standardise_region(Region) else standardise_region(region)
  ) %>%
  select(year, region, total_DDD, population, DDD_population)

Secondary_clean <- Secondary_DDD_by_year_region %>%
  mutate(
    year = as.integer(year),
    region = standardise_region(region)
  ) %>%
  select(year, region, total_DDD, population, DDD_population)

Hospital_clean <- HospitalFP10_DDD_by_year_region %>%
  mutate(
    year = as.integer(year),
    region = standardise_region(region)
  ) %>%
  select(year, region, total_DDD, population, DDD_population)

combined_data <- bind_rows(Primary_clean, Secondary_clean, Hospital_clean)
filtered_data <- combined_data %>% filter(year >= 2019)
summed_data <- filtered_data %>%
  group_by(year) %>%
  summarise(total_DDD_sum = sum(total_DDD, na.rm = TRUE))

max_total_millions <- max(summed_data$total_DDD_sum) / 1e6
national_ddd_plot <- ggplot(summed_data, aes(x = as.integer(year), y = total_DDD_sum / 1e6)) +
  geom_line(color = "#1f78b4", linewidth = 1.2) +
  geom_point(color = "#e31a1c", size = 3) +
  labs(
    title = "National DDD Trends from All Sources",
    subtitle = "Summed total DDD (in millions) across Primary, Secondary, and Hospital FP10 (2019–2024)",
    x = "Year",
    y = "Total DDD (Millions)"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, max_total_millions * 1.1),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(breaks = 2019:2025) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )
ggsave(here(plots_dir, "national_ddd_trends.png"), national_ddd_plot, width = 8, height = 5, dpi = 300)

# Regional DDD trends
Primary_clean_reg <- Primary_DDD_by_year_region %>%
  mutate(
    year = as.integer(year),
    region = if ("Region" %in% names(.)) standardise_region(Region) else standardise_region(region)
  ) %>%
  select(year, region, total_DDD, population, DDD_population)
Secondary_clean_reg <- Secondary_DDD_by_year_region %>%
  mutate(year = as.integer(year), region = standardise_region(region)) %>%
  select(year, region, total_DDD, population, DDD_population)
Hospital_clean_reg <- HospitalFP10_DDD_by_year_region %>%
  mutate(year = as.integer(year), region = standardise_region(region)) %>%
  select(year, region, total_DDD, population, DDD_population)

combined_data_reg <- bind_rows(Primary_clean_reg, Secondary_clean_reg, Hospital_clean_reg)
filtered_data_reg <- combined_data_reg %>% filter(year >= 2019)
summed_by_region <- filtered_data_reg %>%
  group_by(year, region) %>%
  summarise(total_DDD_pop_sum = sum(DDD_population, na.rm = TRUE), .groups = "drop")

regional_trends_plot <- ggplot(summed_by_region, aes(x = year, y = total_DDD_pop_sum, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "DDD Trends by Region (All Data Sources Combined)",
    subtitle = "Summed total DDD per year by region across Primary, Secondary, and Hospital FP10 (2019–Present)",
    x = "Year",
    y = "Total DDD (Millions)/Population",
    color = "Region"
  ) +
  scale_y_continuous(
    limits = c(0, 0.3),
    breaks = c(0.0, 0.05, 0.1, 0.15, 0.2),
    labels = function(x) format(x, nsmall = 1)
  ) +
  scale_x_continuous(breaks = 2019:2025) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )
ggsave(here(plots_dir, "regional_ddd_trends.png"), regional_trends_plot, width = 10, height = 6, dpi = 300)

write.csv(summed_data, here(data_dir, "national_DDD_summed.csv"), row.names = FALSE)
write.csv(summed_by_region, here(data_dir, "regional_DDD_trends.csv"), row.names = FALSE)
write.csv(combined_df_all, here(data_dir, "combined_regional_by_care_2024.csv"), row.names = FALSE)
message("Combined analysis complete. Outputs saved to ", output_dir)
