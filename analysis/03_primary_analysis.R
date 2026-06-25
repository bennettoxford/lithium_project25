source(here::here("analysis", "01_setup.R"))

product_mapping <- read.csv(
  here("data", "primary_care_fp10_products_strength.csv"),
  colClasses = c(bnf_code = "character")
)

practice_regions <- load_ord_practice_regions() %>%
  rename(practice = practice_code)

prescribing_base <- load_epd_lithium() %>%
  mutate(
    month = as.Date(paste0(YEAR_MONTH, "01"), format = "%Y%m%d"),
    practice = PRACTICE_CODE,
    bnf_code = BNF_CODE,
    quantity = TOTAL_QUANTITY,
    epd_region = coalesce(
      normalise_nhs_region(REGIONAL_OFFICE_CODE),
      normalise_nhs_region(REGIONAL_OFFICE_NAME)
    )
  ) %>%
  left_join(practice_regions, by = "practice") %>%
  mutate(
    region = coalesce(region, epd_region, "Unknown")
  ) %>%
  group_by(month, practice, bnf_code, region) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

message("Primary care: ", nrow(prescribing_base), " rows before product mapping join")
after_product <- prescribing_base %>%
  inner_join(
    product_mapping %>%
      select(bnf_code, bnf_name, nm, strnt_nmrtr_val, chemical),
    by = "bnf_code"
  ) %>%
  add_ddd_from_bnf_quantity("quantity") %>%
  mutate(
    year = year(month)
  )
n_unmapped <- nrow(prescribing_base) - nrow(after_product)
message(
  "Primary care: ",
  nrow(after_product),
  " rows after product mapping join (",
  n_unmapped,
  " rows dropped, unmapped BNF)"
)
PRIMARYCARE_dataset <- after_product %>%
  filter(month >= as.Date("2015-01-01"), month <= as.Date("2024-12-31"))
n_out_of_range <- nrow(after_product) - nrow(PRIMARYCARE_dataset)
message(
  "Primary care: ",
  nrow(PRIMARYCARE_dataset),
  " rows after 2015-2024 date filter (",
  n_out_of_range,
  " rows dropped, out of date range)"
)

if (nrow(PRIMARYCARE_dataset) == 0L) {
  stop("No primary care rows after filters.")
}

stop_if_unmapped_regions(
  PRIMARYCARE_dataset,
  id_cols = "practice",
  entity_label = "practice"
)

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
  theme_lithium_trend_line()
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
  theme_lithium_trend_bar()
ggsave(here(plots_dir, "primary_bar_trends.png"), primary_bar, width = 8, height = 5, dpi = 300)

primary_lithium_df <- PRIMARYCARE_dataset %>%
  group_by(region) %>%
  summarise(
    total_DDD = sum(DDD, na.rm = TRUE),
    total_DDD_2024 = sum(DDD[year == 2024L], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = as.factor(region)) %>%
  add_population_for_year(region_col = "region", population_year = 2024L) %>%
  mutate(DDDs_per_1000 = round(total_DDD_2024 / population * 1000, 2))

coverage_data_primary <- nhs_regions_sf %>%
  left_join(primary_lithium_df, by = "region")

primary_label_d <- coverage_map_label_layers_data(coverage_data_primary, "region")
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
  theme_lithium_coverage_map() +
  coord_sf(datum = NA, clip = "off") +
  xlab("") +
  ylab("")
ggsave(here(plots_dir, "primary_coverage_map.png"), primary_coverage_plot, width = 8, height = 6, dpi = 300)

primary_lithium_plot_df <- primary_lithium_df %>%
  filter(is.finite(DDDs_per_1000))

primaryhist <- ggplot(primary_lithium_plot_df, aes(x = region, y = DDDs_per_1000)) +
  geom_col(fill = colour_care_primary, color = colour_care_primary) +
  geom_text(aes(label = sprintf("%.2f", DDDs_per_1000)), vjust = -0.3, size = 3.5) +
  xlab("Region") +
  ylab("DDDs per 1,000 population") +
  scale_y_to_next_tick(
    values = primary_lithium_plot_df$DDDs_per_1000,
    labels = scales::number_format(accuracy = 0.01)
  ) +
  theme_lithium_region_hist()
ggsave(here(plots_dir, "primary_hist_ddd_pop.png"), primaryhist, width = 8, height = 5, dpi = 300)

Primary_DDD_by_year_region <- PRIMARYCARE_dataset %>%
  group_by(year, region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  add_population_by_year(year_col = "year", region_col = "region") %>%
  mutate(DDDs_per_1000 = round(total_DDD / population * 1000, 2))

Primary_DDD_by_year_region_wide <- format_ddd_per_1000_region_year_wide_for_export(
  Primary_DDD_by_year_region
)

write.csv(
  format_ddd_by_year_for_export(Primary_DDD_by_year, "year"),
  here(data_dir, "primary_DDD_by_year.csv"),
  row.names = FALSE
)
write.csv(primary_product_DDD, here(data_dir, "primary_product_DDD.csv"), row.names = FALSE)
write.csv(primary_product_DDD_by_year, here(data_dir, "primary_product_DDD_by_year.csv"), row.names = FALSE)
write.csv(primary_lithium_df, here(data_dir, "primary_lithium_by_region.csv"), row.names = FALSE)
write.csv(Primary_DDD_by_year_region, here(data_dir, "primary_DDD_by_year_region.csv"), row.names = FALSE)
write.csv(Primary_DDD_by_year_region_wide, here(data_dir, "primary_DDD_by_year_region_wide.csv"), row.names = FALSE)
message("Primary care analysis complete. Outputs saved to ", output_dir)
