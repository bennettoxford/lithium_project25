source(here::here("analysis", "01_setup.R"))

product_mapping <- read.csv(
  here("data", "primary_care_fp10_products_strength.csv"),
  colClasses = c(bnf_code = "character")
)

hospital_fp10_data <- load_fp10_monthly()

message("Hospital FP10: ", nrow(hospital_fp10_data), " rows before BNF aggregation")
hospital_fp10_base <- hospital_fp10_data %>%
  group_by(PERIOD, HOSPITAL_TRUST_CODE, HOSPITAL_TRUST, BNF_CODE) %>%
  summarise(
    TOTAL_QUANTITY = sum(TOTAL_QUANTITY, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(bnf_code = BNF_CODE)
message("Hospital FP10: ", nrow(hospital_fp10_base), " rows after BNF aggregation")

trust_mapping <- load_trust_region_mapping()

message("Hospital FP10: ", nrow(hospital_fp10_base), " rows before product mapping join")
n_unmapped <- nrow(hospital_fp10_base) - nrow(
  hospital_fp10_base %>% semi_join(product_mapping, by = "bnf_code")
)
n_mapped_all_periods <- nrow(hospital_fp10_base) - n_unmapped

hospital_fp10_dataset <- hospital_fp10_base %>%
  inner_join(
    product_mapping %>%
      select(bnf_code, bnf_name, nm, strnt_nmrtr_val, chemical),
    by = "bnf_code"
  ) %>%
  add_ddd_from_bnf_quantity("TOTAL_QUANTITY") %>%
  mutate(trust_code_prefix = substr(HOSPITAL_TRUST_CODE, 1, 3)) %>%
  left_join(trust_mapping, by = "trust_code_prefix") %>%
  mutate(
    region = normalise_nhs_region(region),
    year = year(PERIOD)
  ) %>%
  filter(
    PERIOD >= as.Date("2017-01-01"),
    PERIOD <= as.Date("2024-12-31")
  )

message(
  "Hospital FP10: ",
  n_mapped_all_periods,
  " rows after product mapping join (",
  n_unmapped,
  " rows dropped, unmapped BNF)"
)
n_out_of_range <- n_mapped_all_periods - nrow(hospital_fp10_dataset)
message(
  "Hospital FP10: ",
  nrow(hospital_fp10_dataset),
  " rows after 2017-2024 date filter (",
  n_out_of_range,
  " rows dropped, out of date range)"
)

# Y999666 ("UNIDENTIFIED TRUST") is a placeholder for FP10 items that could not be
# matched to a hospital trust. Included in national totals; excluded from regional
# breakdowns.
hospital_fp10_regional <- hospital_fp10_dataset %>%
  filter(trust_code_prefix != "Y99")

stop_if_unmapped_regions(
  hospital_fp10_regional,
  id_cols = c("trust_code_prefix", "HOSPITAL_TRUST_CODE", "HOSPITAL_TRUST"),
  entity_label = "trust"
)

message(
  "Hospital FP10: ",
  format(
    round(
      hospital_fp10_dataset %>%
        filter(trust_code_prefix == "Y99") %>%
        summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
        pull(total_DDD),
      1
    ),
    nsmall = 1
  ),
  " DDD from unidentified trust (Y999666); included in national totals only"
)

if (nrow(hospital_fp10_dataset) == 0L) {
  stop("No hospital FP10 rows after filters.")
}

hospital_fp10_DDD_by_year <- hospital_fp10_dataset %>%
  group_by(year) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop")

hospital_fp10_product_DDD <- hospital_fp10_dataset %>%
  group_by(product_code = bnf_code, product_name = bnf_name) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_DDD), product_name)

hospital_fp10_product_DDD_by_year <- hospital_fp10_dataset %>%
  group_by(year, product_code = bnf_code, product_name = bnf_name) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop")

hospital_fp10_line <- ggplot(hospital_fp10_DDD_by_year, aes(x = year, y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = colour_care_fp10) +
  geom_point(size = 3, color = colour_care_fp10) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = hospital_fp10_DDD_by_year$total_DDD / 1e6,
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(breaks = 2017:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium_trend_line()
ggsave(here(plots_dir, "hospital_fp10_line_trends.png"), hospital_fp10_line, width = 8, height = 5, dpi = 300)

hospital_fp10_DDD_by_year_region <- hospital_fp10_regional %>%
  group_by(year, region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  add_population_by_year(year_col = "year", region_col = "region") %>%
  mutate(DDDs_per_1000 = round(total_DDD / population * 1000, 2))

hospital_fp10_DDD_by_region_2024 <- hospital_fp10_DDD_by_year_region %>%
  filter(year == 2024L) %>%
  transmute(
    region = as.factor(region),
    total_DDD_2024 = total_DDD,
    population,
    DDDs_per_1000
  )

coverage_data_fp10 <- nhs_regions_sf %>%
  left_join(hospital_fp10_DDD_by_region_2024, by = "region")

fp10_label_d <- coverage_map_label_layers_data(coverage_data_fp10, "region")
fp10_label_pts <- dplyr::bind_rows(fp10_label_d$other, fp10_label_d$london_txt)
fp10_label_halo <- coverage_map_label_halo_rect(fp10_label_pts)

hospital_fp10_coverage_plot <- ggplot() +
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
  theme_lithium_coverage_map() +
  coord_sf(datum = NA, clip = "off") +
  xlab("") +
  ylab("")
ggsave(here(plots_dir, "fp10_coverage_map.png"), hospital_fp10_coverage_plot, width = 8, height = 6, dpi = 300)

hospital_fp10_hist <- ggplot(hospital_fp10_DDD_by_region_2024, aes(x = region, y = DDDs_per_1000)) +
  geom_col(fill = colour_care_fp10) +
  geom_text(aes(label = sprintf("%.2f", DDDs_per_1000)), vjust = -0.3, size = 3.5) +
  xlab("Region") +
  ylab("DDDs per 1,000 population") +
  scale_y_to_next_tick(
    values = hospital_fp10_DDD_by_region_2024$DDDs_per_1000,
    labels = scales::number_format(accuracy = 0.01)
  ) +
  theme_lithium_region_hist()
ggsave(here(plots_dir, "fp10_hist_ddd_pop.png"), hospital_fp10_hist, width = 8, height = 5, dpi = 300)

write.csv(
  format_ddd_by_year_for_export(hospital_fp10_DDD_by_year, "year"),
  here(data_dir, "hospital_fp10_DDD_by_year.csv"),
  row.names = FALSE
)
write.csv(hospital_fp10_product_DDD, here(data_dir, "hospital_fp10_product_DDD.csv"), row.names = FALSE)
write.csv(hospital_fp10_product_DDD_by_year, here(data_dir, "hospital_fp10_product_DDD_by_year.csv"), row.names = FALSE)
write.csv(hospital_fp10_DDD_by_region_2024, here(data_dir, "hospital_fp10_DDD_by_region_2024.csv"), row.names = FALSE)
write.csv(hospital_fp10_DDD_by_year_region, here(data_dir, "hospital_fp10_DDD_by_year_region.csv"), row.names = FALSE)
message("FP10 analysis complete. Outputs saved to ", output_dir)
