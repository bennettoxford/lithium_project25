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
primary_product_DDD <- read.csv(here(data_dir, "primary_product_DDD.csv"), colClasses = c(product_code = "character"))
secondary_product_DDD <- read.csv(here(data_dir, "secondary_product_DDD.csv"), colClasses = c(product_code = "character"))
hospital_fp10_product_DDD <- read.csv(here(data_dir, "hospital_fp10_product_DDD.csv"), colClasses = c(product_code = "character"))

primary_care_product_lookup <- read.csv(here("data", "primary_care", "primary_care.csv")) %>%
  select(product_code = bnf_code, generic_product_name = nm) %>%
  distinct(product_code, .keep_all = TRUE)

primary_product_DDD_for_merge <- primary_product_DDD %>%
  group_by(product_code) %>%
  summarise(
    primary_product_name = first(product_name),
    total_DDD_primary_care = sum(total_DDD, na.rm = TRUE),
    .groups = "drop"
  )

hospital_fp10_product_DDD_for_merge <- hospital_fp10_product_DDD %>%
  group_by(product_code) %>%
  summarise(
    fp10_product_name = first(product_name),
    total_DDD_fp10 = sum(total_DDD, na.rm = TRUE),
    .groups = "drop"
  )

primary_fp10_product_DDD <- full_join(
  primary_product_DDD_for_merge,
  hospital_fp10_product_DDD_for_merge,
  by = "product_code"
) %>%
  mutate(
    product_name = coalesce(primary_product_name, fp10_product_name),
    total_DDD_primary_care = replace_na(total_DDD_primary_care, 0),
    total_DDD_fp10 = replace_na(total_DDD_fp10, 0)
  ) %>%
  select(product_code, product_name, total_DDD_primary_care, total_DDD_fp10) %>%
  arrange(desc(total_DDD_primary_care + total_DDD_fp10), product_name)

lithium_products_DDD_summary <- bind_rows(
  primary_product_DDD %>%
    mutate(source = "Primary care", product_code = as.character(product_code)),
  secondary_product_DDD %>%
    mutate(source = "Secondary care", product_code = as.character(product_code)),
  hospital_fp10_product_DDD %>%
    mutate(source = "Hospital FP10", product_code = as.character(product_code))
) %>%
  left_join(primary_care_product_lookup, by = "product_code") %>%
  mutate(product_name = if_else(
    source %in% c("Primary care", "Hospital FP10") & !is.na(generic_product_name),
    generic_product_name,
    product_name
  )) %>%
  select(source, product_code, product_name, total_DDD) %>%
  arrange(source, desc(total_DDD), product_name)

# Combined primary + secondary trends
primary_line <- ggplot(Primaryy_DDD_by_year, aes(x = as.integer(year), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = colour_care_primary) +
  geom_point(size = 3, color = colour_care_primary) +
  labs(x = "Year", y = "Total DDD (millions)", tag = "(a)") +
  scale_y_to_next_tick(
    values = Primaryy_DDD_by_year$total_DDD / 1e6,
    labels = function(x) format(x, scientific = FALSE, big.mark = ",")
  ) +
  scale_x_continuous(breaks = 2015:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.tag = element_text(face = "bold", size = 13),
    plot.tag.position = c(0, 1)
  )

secondary_line <- ggplot(Secondary_DDD_by_year, aes(x = as.integer(year), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = colour_care_secondary) +
  geom_point(size = 3, color = colour_care_secondary) +
  labs(x = "Year", y = "Total DDD (millions)", tag = "(b)") +
  scale_y_to_next_tick(
    values = Secondary_DDD_by_year$total_DDD / 1e6,
    labels = scales::label_number(accuracy = 0.1),
    min_upper = 1.2
  ) +
  scale_x_continuous(breaks = 2019:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.tag = element_text(face = "bold", size = 13),
    plot.tag.position = c(0, 1)
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
            color = colour_care_primary, linewidth = 1.2) +
  geom_point(data = Primaryy_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6),
             color = colour_care_primary, size = 3) +
  geom_line(data = Secondary_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6),
            color = colour_care_secondary, linewidth = 1.2) +
  geom_point(data = Secondary_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6),
             color = colour_care_secondary, size = 3) +
  geom_line(data = HospitalFP10_DDD_by_year,
            aes(x = as.integer(PERIOD), y = total_DDD / 1e6),
            color = colour_care_fp10, linewidth = 1.2) +
  geom_point(data = HospitalFP10_DDD_by_year,
             aes(x = as.integer(PERIOD), y = total_DDD / 1e6),
             color = colour_care_fp10, size = 3) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = c(
      Primaryy_DDD_by_year$total_DDD / 1e6,
      Secondary_DDD_by_year$total_DDD / 1e6,
      HospitalFP10_DDD_by_year$total_DDD / 1e6
    ),
    labels = scales::label_number(accuracy = 1),
    min_upper = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    limits = c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave(here(plots_dir, "combined_line_all_sources.png"), combined_line_plot, width = 10, height = 6, dpi = 300)

combined_line_plot_legend <- ggplot() +
  geom_line(data = Primaryy_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6, color = "Primary care"),
            linewidth = 1.2) +
  geom_point(data = Primaryy_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6, color = "Primary care"),
             size = 3) +
  geom_line(data = Secondary_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6, color = "Secondary care"),
            linewidth = 1.2) +
  geom_point(data = Secondary_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6, color = "Secondary care"),
             size = 3) +
  geom_line(data = HospitalFP10_DDD_by_year,
            aes(x = as.integer(PERIOD), y = total_DDD / 1e6, color = "Hospital FP10"),
            linewidth = 1.2) +
  geom_point(data = HospitalFP10_DDD_by_year,
             aes(x = as.integer(PERIOD), y = total_DDD / 1e6, color = "Hospital FP10"),
             size = 3) +
  scale_color_manual(
    name = "Care Type",
    values = c(
      "Primary care" = colour_care_primary,
      "Secondary care" = colour_care_secondary,
      "Hospital FP10" = colour_care_fp10
    )
  ) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = c(
      Primaryy_DDD_by_year$total_DDD / 1e6,
      Secondary_DDD_by_year$total_DDD / 1e6,
      HospitalFP10_DDD_by_year$total_DDD / 1e6
    ),
    labels = scales::label_number(accuracy = 1),
    min_upper = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    limits = c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(face = "plain"),
    axis.title.y = element_text(face = "plain"),
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
    values = c(
      "Primary" = colour_care_primary,
      "Secondary" = colour_care_secondary,
      "Hospital FP10" = colour_care_fp10
    )
  ) +
  theme_minimal() +
  labs(x = "Region", y = "Lithium usage (DDD/population)", fill = "Care Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(here(plots_dir, "stacked_bar_regional_by_care.png"), stacked_bar_plot, width = 10, height = 6, dpi = 300)

# National DDD trends
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
filtered_data <- combined_data %>% filter(year >= 2019, year <= 2024)
summed_data <- filtered_data %>%
  group_by(year) %>%
  summarise(total_DDD_sum = sum(total_DDD, na.rm = TRUE))

national_ddd_plot <- ggplot(summed_data, aes(x = as.integer(year), y = total_DDD_sum / 1e6)) +
  geom_line(color = colour_care_combined_aggregate, linewidth = 1.2) +
  geom_point(color = colour_care_combined_aggregate, size = 3) +
  labs(x = "Year", y = "Total DDD (Millions)") +
  scale_y_to_next_tick(
    values = summed_data$total_DDD_sum / 1e6,
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(breaks = 2019:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_minimal(base_size = 13)

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
filtered_data_reg <- combined_data_reg %>% filter(year >= 2019, year <= 2024)
summed_by_region <- filtered_data_reg %>%
  group_by(year, region) %>%
  summarise(total_DDD_pop_sum = sum(DDD_population, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(region))

regional_trends_plot <- ggplot(summed_by_region, aes(x = year, y = total_DDD_pop_sum, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(x = "Year", y = "Total DDD (Millions)/Population", color = "Region") +
  scale_colour_nhs_region(drop = FALSE) +
  scale_y_to_next_tick(
    values = summed_by_region$total_DDD_pop_sum,
    labels = scales::number_format(accuracy = 0.01),
    min_upper = 0.3
  ) +
  scale_x_continuous(breaks = 2019:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_minimal(base_size = 13)
ggsave(here(plots_dir, "regional_ddd_trends.png"), regional_trends_plot, width = 10, height = 6, dpi = 300)

write.csv(summed_data, here(data_dir, "national_DDD_summed.csv"), row.names = FALSE)
write.csv(summed_by_region, here(data_dir, "regional_DDD_trends.csv"), row.names = FALSE)
write.csv(combined_df_all, here(data_dir, "combined_regional_by_care_2024.csv"), row.names = FALSE)
write.csv(
  lithium_products_DDD_summary,
  here(data_dir, "lithium_products_DDD_summary.csv"),
  row.names = FALSE
)
write.csv(primary_fp10_product_DDD, here(data_dir, "primary_fp10_product_DDD.csv"), row.names = FALSE)
message("Combined analysis complete. Outputs saved to ", output_dir)
