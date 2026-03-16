source(here::here("analysis", "00_setup.R"))

Lithium_SCMD <- read_excel(here("data", "secondary_care", "lithium-full.xlsx"))
org_mapping <- read_excel(here("data", "secondary_care", "org-mapping.xlsx"))

Lithium_SCMD <- Lithium_SCMD %>%
  left_join(org_mapping %>% select(ods_code, region), by = "ods_code") %>%
  mutate(year_month = as.Date(year_month)) %>%
  mutate(
    total_mg = case_when(
      ingredient_name == "Lithium carbonate" & quantity_basis == "tablet" ~ converted_quantity * strength_numerator_value,
      TRUE ~ NA_real_
    ),
    mmol = case_when(
      ingredient_name == "Lithium carbonate" ~ total_mg / 37.04,
      ingredient_name == "Lithium citrate" & grepl("509", vmp_name) ~ (converted_quantity / 5) * 5.4,
      ingredient_name == "Lithium citrate" & grepl("520", vmp_name) ~ (converted_quantity / 5) * 5.4,
      ingredient_name == "Lithium citrate" & grepl("1.018", vmp_name) ~ (converted_quantity / 5) * 10.8,
      TRUE ~ NA_real_
    ),
    DDD = mmol / 24,
    year = format(as.Date(year_month), "%Y")
  )

Secondary_DDD_by_year <- Lithium_SCMD %>%
  filter(year != "2025") %>%
  group_by(year) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

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
ggsave(here(plots_dir, "secondary_line_trends.png"), secondary_line, width = 8, height = 5, dpi = 300)

secondary_bar <- ggplot(Secondary_DDD_by_year, aes(x = as.factor(year), y = total_DDD / 1e6)) +
  geom_bar(stat = "identity", fill = "#00BFC4", width = 0.6) +
  geom_text(
    aes(label = format(round(total_DDD / 1e6, 1), nsmall = 1)),
    vjust = -0.5,
    size = 4.2,
    fontface = "bold"
  ) +
  labs(
    title = "Secondary Care: Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2019–2024)",
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
ggsave(here(plots_dir, "secondary_bar_trends.png"), secondary_bar, width = 8, height = 5, dpi = 300)

lithium_df <- Lithium_SCMD %>%
  group_by(region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  mutate(region = as.factor(region)) %>%
  filter(!is.na(region))

total_DDD_by_region_2024 <- Lithium_SCMD %>%
  filter(year(year_month) == 2024) %>%
  group_by(region) %>%
  summarise(total_DDD_2024 = sum(DDD, na.rm = TRUE))

secondary_lithium_df <- lithium_df %>%
  left_join(total_DDD_by_region_2024, by = "region") %>%
  left_join(population_df %>% select(region, population), by = "region") %>%
  mutate(`DDD/population` = total_DDD_2024 / population)

coverage_data_secondary <- nhs_regions_sf %>%
  left_join(secondary_lithium_df, by = "region")
unique_values <- sort(unique(coverage_data_secondary$`DDD/population`))
breaks <- c(0, unique_values, max(unique_values, na.rm = TRUE) + 10)
labels <- scales::label_number(breaks)

secondary_coverage_plot <- coverage_data_secondary %>%
  ggplot() +
  geom_sf(aes(fill = `DDD/population`), colour = "black", linewidth = 0.8) +
  geom_sf_text(aes(label = region), colour = "white", size = 3) +
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
ggsave(here(plots_dir, "secondary_coverage_map.png"), secondary_coverage_plot, width = 8, height = 6, dpi = 300)

secondaryhist <- ggplot(secondary_lithium_df, aes(x = region, y = `DDD/population`)) +
  geom_col(fill = "#FF0000", color = "#FF6f6f") +
  geom_text(aes(label = round(`DDD/population`, 3)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (Total DDD for 2024) / population") +
  labs(
    title = "Regional Lithium Use in Secondary Care",
    subtitle = "Average DDDs per Person (2024) (stock movement)"
  ) +
  scale_y_continuous(
    limits = c(0, 0.03),
    breaks = seq(0, 0.03, by = 0.01),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave(here(plots_dir, "secondary_hist_ddd_pop.png"), secondaryhist, width = 8, height = 5, dpi = 300)

Secondary_DDD_by_year_region <- Lithium_SCMD %>%
  filter(year != "2025") %>%
  group_by(year, region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(population_df %>% select(region, population), by = "region") %>%
  mutate(DDD_population = total_DDD / population)

seven_region_secondary <- ggplot(Secondary_DDD_by_year_region, aes(x = as.integer(year), y = DDD_population, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Secondary Care: Lithium Prescribing Trends Per Population",
    subtitle = "DDD per population issued per year by region (2019–2024)",
    x = "Year",
    y = NULL,
    color = "Region"
  ) +
  scale_y_continuous(
    limits = c(0, 0.03),
    expand = c(0, 0),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_x_continuous(breaks = 2019:2024) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )
ggsave(here(plots_dir, "secondary_seven_region_trends.png"), seven_region_secondary, width = 10, height = 6, dpi = 300)

write.csv(Secondary_DDD_by_year, here(data_dir, "secondary_DDD_by_year.csv"), row.names = FALSE)
write.csv(secondary_lithium_df, here(data_dir, "secondary_lithium_by_region.csv"), row.names = FALSE)
write.csv(Secondary_DDD_by_year_region, here(data_dir, "secondary_DDD_by_year_region.csv"), row.names = FALSE)
message("Secondary analysis complete. Outputs saved to ", output_dir)
