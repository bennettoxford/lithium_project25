source(here::here("analysis", "00_setup.R"))

product_mapping <- read.csv(
  here("data", "primary_care_fp10_products_strength.csv"),
  colClasses = c(bnf_code = "character")
)

hospital_fp10_data <- read_excel(
  here("data", "secondary_care_fp10", "FP10_data.xlsx"),
  col_types = "text"
)
hospital_fp10_data <- hospital_fp10_data %>%
  mutate(PERIOD = as.Date(paste0(PERIOD, "01"), format = "%Y%m%d")) %>%
  mutate(
    BNF_CODE            = trimws(coalesce(BNF_CODE, `BNF CODE`)),
    BNF_NAME            = coalesce(BNF_NAME, `BNF NAME`),
    HOSPITAL_TRUST_CODE = coalesce(HOSPITAL_TRUST_CODE, `HOSPITAL TRUST CODE`),
    HOSPITAL_TRUST      = coalesce(HOSPITAL_TRUST, `HOSPITAL TRUST`),
    TOTAL_QUANTITY      = coalesce(TOTAL_QUANTITY, `TOTAL QUANTITY`),
    TOTAL_ITEMS         = coalesce(TOTAL_ITEMS, `TOTAL ITEMS`),
    TOTAL_ACTUAL_COST   = coalesce(TOTAL_ACTUAL_COST, `TOTAL ACTUAL COST`),
    TOTAL_NIC           = coalesce(TOTAL_NIC, `TOTAL NIC`)
  ) %>%
  select(-`BNF CODE`, -`BNF NAME`, -`HOSPITAL TRUST CODE`, -`HOSPITAL TRUST`,
         -`TOTAL QUANTITY`, -`TOTAL ITEMS`, -`TOTAL ACTUAL COST`, -`TOTAL NIC`) %>%
  mutate(across(c(TOTAL_QUANTITY, TOTAL_ITEMS, TOTAL_ACTUAL_COST, TOTAL_NIC), as.numeric)) %>%
  filter(!is.na(PERIOD))

message("Hospital FP10: ", nrow(hospital_fp10_data), " rows before BNF aggregation")
hospital_fp10_base <- hospital_fp10_data %>%
  group_by(PERIOD, HOSPITAL_TRUST_CODE, HOSPITAL_TRUST, BNF_CODE) %>%
  summarise(
    TOTAL_QUANTITY = sum(TOTAL_QUANTITY, na.rm = TRUE),
    TOTAL_ITEMS = sum(TOTAL_ITEMS, na.rm = TRUE),
    TOTAL_ACTUAL_COST = sum(TOTAL_ACTUAL_COST, na.rm = TRUE),
    TOTAL_NIC = sum(TOTAL_NIC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(bnf_code = BNF_CODE)
message("Hospital FP10: ", nrow(hospital_fp10_base), " rows after BNF aggregation")

secondary_care_trusts <- read_csv(here("data", "secondary_care", "secondary_care_trusts.csv"), show_col_types = FALSE)
trust_mapping <- secondary_care_trusts %>%
  mutate(trust_code_prefix = substr(`Trust Code`, 1, 3)) %>%
  select(trust_code_prefix, region = Region) %>%
  distinct(trust_code_prefix, .keep_all = TRUE)

region_mapping <- c(
  "RCE" = "North East And Yorkshire",
  "RK7" = "North East And Yorkshire",
  "RQ4" = "Midlands",
  "RRQ" = "London",
  "RNJ" = "London",
  "RMK" = "North West",
  "RNH" = "London",
  "RGC" = "London", # Whipps Cross
  "RNK" = "London", # Tavistock & Portman
  "RWN" = "East of England" # South Essex Partnership
)

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
    region = if_else(
      is.na(region) & trust_code_prefix %in% names(region_mapping),
      region_mapping[trust_code_prefix],
      region
    ),
    Region = normalise_nhs_region(region),
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
  theme_lithium(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave(here(plots_dir, "hospital_fp10_line_trends.png"), hospital_fp10_line, width = 8, height = 5, dpi = 300)

hospital_fp10_DDD_by_year_region <- hospital_fp10_regional %>%
  group_by(year, Region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = "drop") %>%
  add_population_by_year(year_col = "year", region_col = "Region") %>%
  mutate(DDDs_per_1000 = round(total_DDD / population * 1000, 2))

hospital_fp10_DDD_by_region_2024 <- hospital_fp10_DDD_by_year_region %>%
  filter(year == 2024L) %>%
  transmute(
    Region = as.factor(Region),
    total_DDD_2024 = total_DDD,
    population,
    DDDs_per_1000
  )

coverage_data_fp10 <- nhs_regions_sf %>%
  left_join(hospital_fp10_DDD_by_region_2024, by = "Region")

fp10_label_d <- coverage_map_label_layers_data(coverage_data_fp10, "Region")
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
ggsave(here(plots_dir, "fp10_coverage_map.png"), hospital_fp10_coverage_plot, width = 8, height = 6, dpi = 300)

hospital_fp10_hist <- ggplot(hospital_fp10_DDD_by_region_2024, aes(x = Region, y = DDDs_per_1000)) +
  geom_col(fill = colour_care_fp10) +
  geom_text(aes(label = sprintf("%.2f", DDDs_per_1000)), vjust = -0.3, size = 3.5) +
  theme_lithium() +
  xlab("Region") +
  ylab("DDDs per 1,000 population") +
  scale_y_to_next_tick(
    values = hospital_fp10_DDD_by_region_2024$DDDs_per_1000,
    labels = scales::number_format(accuracy = 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = axis_tick_label_size),
    axis.text.y = element_text(size = axis_tick_label_size),
    plot.margin = margin(10, 10, 10, 10)
  )
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
