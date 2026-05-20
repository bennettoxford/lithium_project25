source(here::here("analysis", "00_setup.R"))

# Import primary care dataset
Practice_codes <- read_excel(
  here("data", "primary_care", "practice_codes.xlsx"),
  col_types = c(
    rep("text", 11),
    "numeric",
    rep("text", 5)
  )
)
product_mapping <- read.csv(
  here("data", "primary_care_fp10_products_strength.csv"),
  colClasses = c(bnf_code = "character")
)

PRIMARYCARE_dataset <- read.csv(
  gzfile(here("data", "primary_care", "primary_lithium.csv.gz")),
  colClasses = c(bnf_code = "character")
) %>%
  mutate(month = as.Date(month)) %>%
  group_by(month, practice, sicbl, icb, regional_team, bnf_code) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    items = sum(items, na.rm = TRUE),
    actual_cost = sum(actual_cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(Practice_codes, by = c("practice" = "code")) %>%
  filter(setting == 4) %>% # GP practices only
  inner_join(
    product_mapping %>%
      select(bnf_code, bnf_name, nm, strnt_nmrtr_val, chemical),
    by = "bnf_code"
  ) %>%
  mutate(
    quantity_mg = quantity * strnt_nmrtr_val,
    mmol = case_when(
      chemical == "Lithium Carbonate" ~ quantity_mg / 37.04,
      chemical == "Lithium Citrate" ~ quantity_mg / 94.26,
      TRUE ~ NA_real_
    ),
    DDD = mmol / 24,
    Region = nhser_to_Region[as.character(regional_team)],
    year = year(month)
  ) %>%
  filter(month >= as.Date("2015-01-01"), month <= as.Date("2024-12-31"))

if (nrow(PRIMARYCARE_dataset) == 0L) {
  stop("No primary care rows after filters.")
}

Primary_DDD_by_year <- PRIMARYCARE_dataset %>%
  group_by(year) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop")

primary_product_DDD <- PRIMARYCARE_dataset %>%
  group_by(product_code = bnf_code, product_name = bnf_name) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_DDD), product_name)

primary_product_DDD_by_year <- PRIMARYCARE_dataset %>%
  group_by(year, product_code = bnf_code, product_name = bnf_name) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop")

primary_line <- ggplot(Primary_DDD_by_year, aes(x = year, y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = colour_care_primary) +
  geom_point(size = 3, color = colour_care_primary) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = Primary_DDD_by_year$total_DDD / 1e6,
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(breaks = 2015:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave(here(plots_dir, "primary_line_trends.png"), primary_line, width = 8, height = 5, dpi = 300)

primary_bar <- ggplot(Primary_DDD_by_year, aes(x = as.factor(year), y = total_DDD / 1e6)) +
  geom_bar(stat = "identity", fill = colour_care_primary, width = 0.6) +
  geom_text(
    aes(label = format(round(total_DDD / 1e6, 1), nsmall = 1)),
    vjust = -0.5,
    size = 4.2,
    fontface = "bold"
  ) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = Primary_DDD_by_year$total_DDD / 1e6,
    labels = function(x) format(x, scientific = FALSE, big.mark = ",")
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )
ggsave(here(plots_dir, "primary_bar_trends.png"), primary_bar, width = 8, height = 5, dpi = 300)

primary_lithium_df <- PRIMARYCARE_dataset %>%
  filter(!is.na(Region)) %>%
  group_by(Region) %>%
  summarise(
    total_DDD = sum(DDD, na.rm = TRUE),
    total_DDD_2024 = sum(DDD[year == 2024L], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Region = as.factor(Region)) %>%
  add_population_for_year(region_col = "Region", population_year = 2024L) %>%
  mutate(DDDs_per_1000 = round(total_DDD_2024 / population * 1000, 2))

coverage_data_primary <- nhs_regions_sf %>%
  left_join(primary_lithium_df, by = "Region")

primary_label_d <- coverage_map_label_layers_data(coverage_data_primary, "Region")
primary_label_pts <- dplyr::bind_rows(primary_label_d$other, primary_label_d$london_txt)
primary_label_halo <- coverage_map_label_halo_rect(primary_label_pts)

primary_coverage_plot <- ggplot() +
  geom_sf(data = coverage_data_primary, aes(fill = DDDs_per_1000), colour = "black", linewidth = 0.8) +
  geom_segment(
    data = primary_label_d$london_seg,
    aes(x = lon, y = lat, xend = lon_end, yend = lat_end),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = coverage_map_leader_linewidth,
    lineend = "round"
  ) +
  geom_polygon(
    data = primary_label_halo,
    aes(x = lon, y = lat, group = group),
    inherit.aes = FALSE,
    fill = "white",
    colour = "grey25",
    linewidth = 0.35
  ) +
  geom_text(
    data = primary_label_pts,
    aes(x = lon, y = lat, label = label),
    inherit.aes = FALSE,
    colour = "black",
    fontface = "bold",
    size = coverage_map_value_label_size
  ) +
  scale_fill_gradientn(
    colors = colour_care_primary_map,
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
ggsave(here(plots_dir, "primary_coverage_map.png"), primary_coverage_plot, width = 8, height = 6, dpi = 300)

primaryhist <- ggplot(primary_lithium_df, aes(x = Region, y = DDDs_per_1000)) +
  geom_col(fill = colour_care_primary, color = colour_care_primary) +
  geom_text(aes(label = sprintf("%.2f", DDDs_per_1000)), vjust = -0.3, size = 3.5) +
  theme_lithium() +
  xlab("Region") +
  ylab("DDDs per 1,000 population") +
  scale_y_to_next_tick(
    values = primary_lithium_df$DDDs_per_1000,
    labels = scales::number_format(accuracy = 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = axis_tick_label_size),
    axis.text.y = element_text(size = axis_tick_label_size),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave(here(plots_dir, "primary_hist_ddd_pop.png"), primaryhist, width = 8, height = 5, dpi = 300)

Primary_DDD_by_year_region <- PRIMARYCARE_dataset %>%
  group_by(year, Region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  add_population_by_year(year_col = "year", region_col = "Region") %>%
  mutate(DDDs_per_1000 = round(total_DDD / population * 1000, 2))

write.csv(
  format_ddd_by_year_for_export(Primary_DDD_by_year, "year"),
  here(data_dir, "primary_DDD_by_year.csv"),
  row.names = FALSE
)
write.csv(primary_product_DDD, here(data_dir, "primary_product_DDD.csv"), row.names = FALSE)
write.csv(primary_product_DDD_by_year, here(data_dir, "primary_product_DDD_by_year.csv"), row.names = FALSE)
write.csv(primary_lithium_df, here(data_dir, "primary_lithium_by_region.csv"), row.names = FALSE)
write.csv(Primary_DDD_by_year_region, here(data_dir, "primary_DDD_by_year_region.csv"), row.names = FALSE)
message("Primary analysis complete. Outputs saved to ", output_dir)
