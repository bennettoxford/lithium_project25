source(here::here("analysis", "00_setup.R"))

secondary_care <- read_csv(here("data", "secondary_care", "secondary_care.csv"), show_col_types = FALSE)

Lithium_SCMD <- secondary_care %>%
  mutate(
    year_month = as.Date(Date),
    year = format(year_month, "%Y"),
    region = Region,
    DDD = Value  # already in DDDs
  ) %>%
  filter(year_month <= as.Date("2024-12-31"))

Secondary_DDD_by_year <- Lithium_SCMD %>%
  group_by(year) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

secondary_product_DDD <- Lithium_SCMD %>%
  group_by(product_code = `VMP Code`, product_name = `VMP Name`) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_DDD), product_name)

secondary_line <- ggplot(Secondary_DDD_by_year, aes(x = as.integer(year), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = colour_care_secondary) +
  geom_point(size = 3, color = colour_care_secondary) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = Secondary_DDD_by_year$total_DDD / 1e6,
    labels = scales::label_number(accuracy = 0.1),
    min_upper = 1.2
  ) +
  scale_x_continuous(breaks = 2019:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave(here(plots_dir, "secondary_line_trends.png"), secondary_line, width = 8, height = 5, dpi = 300)

secondary_bar <- ggplot(Secondary_DDD_by_year, aes(x = as.factor(year), y = total_DDD / 1e6)) +
  geom_bar(stat = "identity", fill = colour_care_secondary, width = 0.6) +
  geom_text(
    aes(label = format(round(total_DDD / 1e6, 1), nsmall = 1)),
    vjust = -0.5,
    size = 4.2,
    fontface = "bold"
  ) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = Secondary_DDD_by_year$total_DDD / 1e6,
    labels = function(x) format(x, scientific = FALSE, big.mark = ",")
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium(base_size = 13) +
  theme(
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
  mutate(DDDs_per_1000 = round(total_DDD_2024 / population * 1000, 2))

coverage_data_secondary <- nhs_regions_sf %>%
  left_join(secondary_lithium_df, by = "region")

secondary_label_d <- coverage_map_label_layers_data(coverage_data_secondary, "region")
secondary_label_pts <- dplyr::bind_rows(secondary_label_d$other, secondary_label_d$london_txt)
secondary_label_halo <- coverage_map_label_halo_rect(secondary_label_pts)

secondary_coverage_plot <- ggplot() +
  geom_sf(data = coverage_data_secondary, aes(fill = DDDs_per_1000), colour = "black", linewidth = 0.8) +
  geom_segment(
    data = secondary_label_d$london_seg,
    aes(x = lon, y = lat, xend = lon_end, yend = lat_end),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = coverage_map_leader_linewidth,
    lineend = "round"
  ) +
  geom_polygon(
    data = secondary_label_halo,
    aes(x = lon, y = lat, group = group),
    inherit.aes = FALSE,
    fill = "white",
    colour = "grey25",
    linewidth = 0.35
  ) +
  geom_text(
    data = secondary_label_pts,
    aes(x = lon, y = lat, label = label),
    inherit.aes = FALSE,
    colour = "black",
    fontface = "bold",
    size = coverage_map_value_label_size
  ) +
  scale_fill_gradientn(
    colors = colour_care_secondary_map,
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
ggsave(here(plots_dir, "secondary_coverage_map.png"), secondary_coverage_plot, width = 8, height = 6, dpi = 300)

secondaryhist <- ggplot(secondary_lithium_df, aes(x = region, y = DDDs_per_1000)) +
  geom_col(fill = colour_care_secondary, color = colour_care_secondary) +
  geom_text(aes(label = sprintf("%.2f", DDDs_per_1000)), vjust = -0.3, size = 3.5) +
  theme_lithium() +
  xlab("Region") +
  ylab("DDDs per 1,000 population") +
  scale_y_to_next_tick(
    values = secondary_lithium_df$DDDs_per_1000,
    labels = scales::number_format(accuracy = 0.01),
    min_upper = 30
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = axis_tick_label_size),
    axis.text.y = element_text(size = axis_tick_label_size),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave(here(plots_dir, "secondary_hist_ddd_pop.png"), secondaryhist, width = 8, height = 5, dpi = 300)

Secondary_DDD_by_year_region <- Lithium_SCMD %>%
  group_by(year, region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(population_df %>% select(region, population), by = "region") %>%
  mutate(DDDs_per_1000 = round(total_DDD / population * 1000, 2))

seven_region_secondary <- Secondary_DDD_by_year_region %>%
  mutate(region = standardise_region(as.character(region))) %>%
  ggplot(aes(x = as.integer(year), y = DDDs_per_1000, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(x = "Year", y = "DDDs per 1,000 population", color = "Region") +
  scale_colour_nhs_region(drop = FALSE) +
  scale_y_to_next_tick(
    values = Secondary_DDD_by_year_region$DDDs_per_1000,
    labels = scales::number_format(accuracy = 0.01),
    min_upper = 30
  ) +
  scale_x_continuous(breaks = 2019:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )
ggsave(here(plots_dir, "secondary_seven_region_trends.png"), seven_region_secondary, width = 10, height = 6, dpi = 300)

write.csv(
  format_ddd_by_year_for_export(Secondary_DDD_by_year, "year"),
  here(data_dir, "secondary_DDD_by_year.csv"),
  row.names = FALSE
)
write.csv(secondary_product_DDD, here(data_dir, "secondary_product_DDD.csv"), row.names = FALSE)
write.csv(secondary_lithium_df, here(data_dir, "secondary_lithium_by_region.csv"), row.names = FALSE)
write.csv(Secondary_DDD_by_year_region, here(data_dir, "secondary_DDD_by_year_region.csv"), row.names = FALSE)
message("Secondary analysis complete. Outputs saved to ", output_dir)
