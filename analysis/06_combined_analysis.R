source(here::here("analysis", "01_setup.R"))

Primary_DDD_by_year <- read_ddd_by_year_export_csv(here(data_dir, "primary_DDD_by_year.csv"))
Secondary_DDD_by_year <- read_ddd_by_year_export_csv(here(data_dir, "secondary_DDD_by_year.csv"))
hospital_fp10_DDD_by_year <- read_ddd_by_year_export_csv(here(data_dir, "hospital_fp10_DDD_by_year.csv")) %>%
  mutate(PERIOD = year)
primary_lithium_df <- read.csv(here(data_dir, "primary_lithium_by_region.csv")) %>%
  ensure_region_column()
secondary_lithium_df <- read.csv(here(data_dir, "secondary_lithium_by_region.csv")) %>%
  ensure_region_column()
hospital_fp10_DDD_by_region_2024 <- read.csv(here(data_dir, "hospital_fp10_DDD_by_region_2024.csv")) %>%
  ensure_region_column()
Primary_DDD_by_year_region <- read.csv(here(data_dir, "primary_DDD_by_year_region.csv")) %>%
  ensure_region_column()
Secondary_DDD_by_year_region <- read.csv(here(data_dir, "secondary_DDD_by_year_region.csv")) %>%
  ensure_region_column()
hospital_fp10_DDD_by_year_region <- read.csv(here(data_dir, "hospital_fp10_DDD_by_year_region.csv")) %>%
  ensure_region_column()
primary_product_DDD <- read.csv(here(data_dir, "primary_product_DDD.csv"), colClasses = c(product_code = "character"))
secondary_product_DDD <- read.csv(here(data_dir, "secondary_product_DDD.csv"), colClasses = c(product_code = "character"))
hospital_fp10_product_DDD <- read.csv(here(data_dir, "hospital_fp10_product_DDD.csv"), colClasses = c(product_code = "character"))
primary_product_DDD_by_year <- read.csv(here(data_dir, "primary_product_DDD_by_year.csv"), colClasses = c(product_code = "character", year = "character"))
secondary_product_DDD_by_year <- read.csv(here(data_dir, "secondary_product_DDD_by_year.csv"), colClasses = c(product_code = "character", year = "character"))
hospital_fp10_product_DDD_by_year <- read.csv(here(data_dir, "hospital_fp10_product_DDD_by_year.csv"), colClasses = c(product_code = "character", year = "character"))

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

lithium_product_ddd_summary_from_by_year <- function(df, source_label) {
  df <- df %>%
    mutate(product_code = as.character(product_code), year_int = as.integer(year))
  first_yr <- min(df$year_int, na.rm = TRUE)
  last_yr <- max(df$year_int, na.rm = TRUE)
  d_all <- df %>%
    group_by(product_code) %>%
    summarise(
      product_name = first(product_name),
      total_DDD = sum(total_DDD, na.rm = TRUE),
      .groups = "drop"
    )
  d_first <- df %>%
    filter(year_int == first_yr) %>%
    group_by(product_code) %>%
    summarise(total_DDD_first_year = sum(total_DDD, na.rm = TRUE), .groups = "drop")
  d_last <- df %>%
    filter(year_int == last_yr) %>%
    group_by(product_code) %>%
    summarise(total_DDD_last_year = sum(total_DDD, na.rm = TRUE), .groups = "drop")
  d_all %>%
    left_join(d_first, by = "product_code") %>%
    left_join(d_last, by = "product_code") %>%
    mutate(
      source = source_label,
      first_data_year = first_yr,
      last_data_year = last_yr,
      total_DDD_first_year = replace_na(total_DDD_first_year, 0),
      total_DDD_last_year = replace_na(total_DDD_last_year, 0)
    )
}

ddd_by_product_in_year <- function(df, y) {
  df %>%
    mutate(year_int = as.integer(year)) %>%
    filter(year_int == y) %>%
    group_by(product_code) %>%
    summarise(ddd = sum(total_DDD, na.rm = TRUE), .groups = "drop") %>%
    mutate(product_code = as.character(product_code))
}

primary_care_first_yr <- min(as.integer(primary_product_DDD_by_year$year), na.rm = TRUE)
primary_care_last_yr <- max(as.integer(primary_product_DDD_by_year$year), na.rm = TRUE)
fp10_first_yr <- min(as.integer(hospital_fp10_product_DDD_by_year$year), na.rm = TRUE)
fp10_last_yr <- max(as.integer(hospital_fp10_product_DDD_by_year$year), na.rm = TRUE)

prim_first <- ddd_by_product_in_year(primary_product_DDD_by_year, primary_care_first_yr)
fp10_first <- ddd_by_product_in_year(hospital_fp10_product_DDD_by_year, fp10_first_yr)
prim_last <- ddd_by_product_in_year(primary_product_DDD_by_year, primary_care_last_yr)
fp10_last <- ddd_by_product_in_year(hospital_fp10_product_DDD_by_year, fp10_last_yr)

format_ddd_int_comma <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    format(as.integer(round(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

secondary_summary_for_table <- lithium_product_ddd_summary_from_by_year(
  secondary_product_DDD_by_year,
  "Secondary care"
)

secondary_earliest_yr <- as.integer(secondary_summary_for_table$first_data_year[1L])
secondary_latest_yr <- as.integer(secondary_summary_for_table$last_data_year[1L])

lithium_primary_fp10_detail <- primary_fp10_product_DDD %>%
  mutate(
    product_code = as.character(product_code),
    total_DDD_all_periods = replace_na(total_DDD_primary_care, 0) + replace_na(total_DDD_fp10, 0)
  ) %>%
  left_join(prim_first %>% rename(prim_first_ddd = ddd), by = "product_code") %>%
  left_join(fp10_first %>% rename(fp10_first_ddd = ddd), by = "product_code") %>%
  left_join(prim_last %>% rename(prim_last_ddd = ddd), by = "product_code") %>%
  left_join(fp10_last %>% rename(fp10_last_ddd = ddd), by = "product_code") %>%
  arrange(desc(total_DDD_all_periods), product_name)

primary_fp10_total_row <- lithium_primary_fp10_detail %>%
  summarise(
    prim_first_ddd = sum(replace_na(prim_first_ddd, 0)),
    prim_last_ddd = sum(replace_na(prim_last_ddd, 0)),
    fp10_first_ddd = sum(replace_na(fp10_first_ddd, 0)),
    fp10_last_ddd = sum(replace_na(fp10_last_ddd, 0)),
    .groups = "drop"
  ) %>%
  transmute(
    V1 = NA_character_,
    V2 = "Total (all products)",
    V3 = format_ddd_int_comma(prim_first_ddd),
    V4 = format_ddd_int_comma(prim_last_ddd),
    V5 = format_ddd_int_comma(fp10_first_ddd),
    V6 = format_ddd_int_comma(fp10_last_ddd)
  )

lithium_primary_fp10_summary_rows <- lithium_primary_fp10_detail %>%
  transmute(
    V1 = NA_character_,
    V2 = paste0(product_name, " (", product_code, ")"),
    V3 = format_ddd_int_comma(replace_na(prim_first_ddd, 0)),
    V4 = format_ddd_int_comma(replace_na(prim_last_ddd, 0)),
    V5 = format_ddd_int_comma(replace_na(fp10_first_ddd, 0)),
    V6 = format_ddd_int_comma(replace_na(fp10_last_ddd, 0))
  )

secondary_summary_detail <- secondary_summary_for_table %>%
  arrange(desc(total_DDD), product_name)

secondary_total_row <- secondary_summary_detail %>%
  summarise(
    total_DDD_first_year = sum(total_DDD_first_year, na.rm = TRUE),
    total_DDD_last_year = sum(total_DDD_last_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    V1 = NA_character_,
    V2 = "Total (all products)",
    V3 = format_ddd_int_comma(total_DDD_first_year),
    V4 = NA_character_,
    V5 = format_ddd_int_comma(total_DDD_last_year),
    V6 = NA_character_
  )

secondary_summary_rows <- secondary_summary_detail %>%
  transmute(
    V1 = NA_character_,
    V2 = paste0(product_name, " (", as.character(product_code), ")"),
    V3 = format_ddd_int_comma(total_DDD_first_year),
    V4 = NA_character_,
    V5 = format_ddd_int_comma(total_DDD_last_year),
    V6 = NA_character_
  )

lithium_products_DDD_summary_csv <- bind_rows(
  as_tibble(list(
    V1 = "Primary care/community prescribing (FP10)",
    V2 = "BNF name (BNF code)",
    V3 = "Total DDDs",
    V4 = NA_character_,
    V5 = NA_character_,
    V6 = NA_character_
  )),
  as_tibble(list(
    V1 = NA_character_,
    V2 = NA_character_,
    V3 = "Primary care",
    V4 = NA_character_,
    V5 = "Community prescribing (FP10)",
    V6 = NA_character_
  )),
  as_tibble(list(
    V1 = NA_character_,
    V2 = NA_character_,
    V3 = as.character(primary_care_first_yr),
    V4 = as.character(primary_care_last_yr),
    V5 = as.character(fp10_first_yr),
    V6 = as.character(fp10_last_yr)
  )),
  primary_fp10_total_row,
  lithium_primary_fp10_summary_rows,
  as_tibble(list(
    V1 = "Secondary care",
    V2 = NA_character_,
    V3 = NA_character_,
    V4 = NA_character_,
    V5 = NA_character_,
    V6 = NA_character_
  )),
  as_tibble(list(
    V1 = NA_character_,
    V2 = NA_character_,
    V3 = as.character(secondary_earliest_yr),
    V4 = NA_character_,
    V5 = as.character(secondary_latest_yr),
    V6 = NA_character_
  )),
  secondary_total_row,
  secondary_summary_rows
)

total_ddd_in_year <- function(by_year_df, yr) {
  by_year_df %>%
    filter(as.integer(.data$year) == yr) %>%
    summarise(total = sum(.data$total_DDD, na.rm = TRUE), .groups = "drop") %>%
    pull(total)
}

lithium_DDD_summary_by_source <- bind_rows(
  tibble(
    Source = "Primary care",
    `Number of products` = n_distinct(as.character(primary_product_DDD$product_code)),
    `First year` = primary_care_first_yr,
    `Total DDDs (first year)` = total_ddd_in_year(Primary_DDD_by_year, primary_care_first_yr),
    `Total DDDs (2024)` = total_ddd_in_year(Primary_DDD_by_year, primary_care_last_yr),
    `Total DDDs (all years)` = sum(Primary_DDD_by_year$total_DDD, na.rm = TRUE)
  ),
  tibble(
    Source = "Hospital FP10",
    `Number of products` = n_distinct(as.character(hospital_fp10_product_DDD$product_code)),
    `First year` = fp10_first_yr,
    `Total DDDs (first year)` = total_ddd_in_year(hospital_fp10_DDD_by_year, fp10_first_yr),
    `Total DDDs (2024)` = total_ddd_in_year(hospital_fp10_DDD_by_year, fp10_last_yr),
    `Total DDDs (all years)` = sum(hospital_fp10_DDD_by_year$total_DDD, na.rm = TRUE)
  ),
  tibble(
    Source = "Secondary care",
    `Number of products` = n_distinct(as.character(secondary_product_DDD$product_code)),
    `First year` = secondary_earliest_yr,
    `Total DDDs (first year)` = total_ddd_in_year(Secondary_DDD_by_year, secondary_earliest_yr),
    `Total DDDs (2024)` = total_ddd_in_year(Secondary_DDD_by_year, secondary_latest_yr),
    `Total DDDs (all years)` = sum(Secondary_DDD_by_year$total_DDD, na.rm = TRUE)
  )
)

# Combined primary + secondary trends
primary_line <- ggplot(Primary_DDD_by_year, aes(x = as.integer(year), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = colour_care_primary) +
  geom_point(size = 3, color = colour_care_primary) +
  labs(x = "Year", y = "Total DDD (millions)", tag = "(a)") +
  scale_y_to_next_tick(
    values = Primary_DDD_by_year$total_DDD / 1e6,
    labels = function(x) format(x, scientific = FALSE, big.mark = ",")
  ) +
  scale_x_continuous(breaks = 2015:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium(base_size = 13) +
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
  theme_lithium(base_size = 13) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.tag = element_text(face = "bold", size = 13),
    plot.tag.position = c(0, 1)
  )

combined_plot <- primary_line / secondary_line
ggsave(here(plots_dir, "combined_primary_secondary_trends.png"), combined_plot, width = 8, height = 10, dpi = 300)

coverage_map_panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 18),
  plot.tag.position = c(0, 1)
)

primary_coverage_map_combined <- primary_coverage_plot +
  labs(tag = "(a)") +
  coverage_map_panel_tag_theme +
  theme(plot.margin = margin(
    5.5,
    coverage_map_plot_margin_right,
    5.5,
    coverage_map_combined_margin_left_pt
  ))
secondary_coverage_map_combined <- secondary_coverage_plot +
  labs(tag = "(b)") +
  coverage_map_panel_tag_theme
fp10_coverage_map_combined <- hospital_fp10_coverage_plot +
  labs(tag = "(c)") +
  coverage_map_panel_tag_theme +
  theme(plot.margin = margin(
    5.5,
    coverage_map_plot_margin_right,
    5.5,
    coverage_map_combined_margin_left_pt
  ))

combined_coverage_maps <- wrap_plots(
  primary_coverage_map_combined,
  secondary_coverage_map_combined,
  fp10_coverage_map_combined,
  plot_spacer(),
  ncol = 2,
  nrow = 2
) +
  plot_layout(widths = c(1, 1), heights = c(1, 1))
ggsave(
  here(plots_dir, "combined_coverage_maps.png"),
  combined_coverage_maps,
  width = 16,
  height = 12,
  dpi = 300
)

# Combined line plot (all three sources)
all_years <- c(
  as.numeric(Primary_DDD_by_year$year),
  as.numeric(Secondary_DDD_by_year$year),
  as.numeric(hospital_fp10_DDD_by_year$PERIOD)
)

combined_totals_by_year <- full_join(
  Primary_DDD_by_year %>%
    transmute(year = as.integer(year), primary_total_DDD = total_DDD),
  Secondary_DDD_by_year %>%
    transmute(year = as.integer(year), secondary_total_DDD = total_DDD),
  by = "year"
) %>%
  full_join(
    hospital_fp10_DDD_by_year %>%
      transmute(year = as.integer(PERIOD), fp10_total_DDD = total_DDD),
    by = "year"
  ) %>%
  mutate(
    primary_total_DDD = replace_na(primary_total_DDD, 0),
    secondary_total_DDD = replace_na(secondary_total_DDD, 0),
    fp10_total_DDD = replace_na(fp10_total_DDD, 0),
    total_DDD = primary_total_DDD + secondary_total_DDD + fp10_total_DDD
  ) %>%
  filter(year >= 2019) %>%
  arrange(year)

combined_line_plot <- ggplot() +
  geom_line(data = Primary_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6),
            color = colour_care_primary, linewidth = 1.2) +
  geom_point(data = Primary_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6),
             color = colour_care_primary, size = 3) +
  geom_line(data = Secondary_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6),
            color = colour_care_secondary, linewidth = 1.2) +
  geom_point(data = Secondary_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6),
             color = colour_care_secondary, size = 3) +
  geom_line(data = hospital_fp10_DDD_by_year,
            aes(x = as.integer(PERIOD), y = total_DDD / 1e6),
            color = colour_care_fp10, linewidth = 1.2) +
  geom_point(data = hospital_fp10_DDD_by_year,
             aes(x = as.integer(PERIOD), y = total_DDD / 1e6),
             color = colour_care_fp10, size = 3) +
  geom_line(data = combined_totals_by_year,
            aes(x = year, y = total_DDD / 1e6),
            color = colour_care_combined_aggregate, linewidth = 1.2) +
  geom_point(data = combined_totals_by_year,
             aes(x = year, y = total_DDD / 1e6),
             color = colour_care_combined_aggregate, size = 3) +
  labs(x = "Year", y = "Total DDD (millions)") +
  scale_y_to_next_tick(
    values = c(
      Primary_DDD_by_year$total_DDD / 1e6,
      Secondary_DDD_by_year$total_DDD / 1e6,
      hospital_fp10_DDD_by_year$total_DDD / 1e6,
      combined_totals_by_year$total_DDD / 1e6
    ),
    labels = scales::label_number(accuracy = 1),
    min_upper = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    limits = c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_lithium(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
ggsave(here(plots_dir, "combined_line_all_sources.png"), combined_line_plot, width = 10, height = 6, dpi = 300)

combined_line_plot_legend <- ggplot() +
  geom_line(data = Primary_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6, color = "Primary care"),
            linewidth = 1.2) +
  geom_point(data = Primary_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6, color = "Primary care"),
             size = 3) +
  geom_line(data = Secondary_DDD_by_year,
            aes(x = as.integer(year), y = total_DDD / 1e6, color = "Secondary care"),
            linewidth = 1.2) +
  geom_point(data = Secondary_DDD_by_year,
             aes(x = as.integer(year), y = total_DDD / 1e6, color = "Secondary care"),
             size = 3) +
  geom_line(data = hospital_fp10_DDD_by_year,
            aes(x = as.integer(PERIOD), y = total_DDD / 1e6, color = "Hospital FP10"),
            linewidth = 1.2) +
  geom_point(data = hospital_fp10_DDD_by_year,
             aes(x = as.integer(PERIOD), y = total_DDD / 1e6, color = "Hospital FP10"),
             size = 3) +
  geom_line(data = combined_totals_by_year,
            aes(x = year, y = total_DDD / 1e6, color = "Total"),
            linewidth = 1.2) +
  geom_point(data = combined_totals_by_year,
             aes(x = year, y = total_DDD / 1e6, color = "Total"),
             size = 3) +
  scale_color_manual(
    name = "Care Type",
    values = c(
      "Primary care" = colour_care_primary,
      "Secondary care" = colour_care_secondary,
      "Hospital FP10" = colour_care_fp10,
      "Total" = colour_care_combined_aggregate
    )
  ) +
  labs(x = "Year", y = "DDDs (millions)") +
  scale_y_to_next_tick(
    values = c(
      Primary_DDD_by_year$total_DDD / 1e6,
      Secondary_DDD_by_year$total_DDD / 1e6,
      hospital_fp10_DDD_by_year$total_DDD / 1e6,
      combined_totals_by_year$total_DDD / 1e6
    ),
    labels = scales::label_number(accuracy = 1),
    min_upper = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    limits = c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme_lithium(base_size = 13) +
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
  mutate(Source = "Primary")
secondary_lithium_df <- secondary_lithium_df %>%
  mutate(Source = "Secondary")
hospital_fp10_DDD_by_region_2024 <- hospital_fp10_DDD_by_region_2024 %>%
  mutate(Source = "Hospital FP10")

combined_df_all <- bind_rows(primary_lithium_df, secondary_lithium_df, hospital_fp10_DDD_by_region_2024) %>%
  mutate(Source = factor(Source, levels = c("Primary", "Secondary", "Hospital FP10"))) %>%
  select(
    region,
    population,
    total_DDD_2024,
    DDDs_per_1000,
    Source
  )

stacked_bar_plot <- ggplot(combined_df_all, aes(x = region, y = DDDs_per_1000, fill = Source)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_fill_manual(
    values = c(
      "Primary" = colour_care_primary,
      "Secondary" = colour_care_secondary,
      "Hospital FP10" = colour_care_fp10
    )
  ) +
  theme_lithium() +
  labs(x = "Region", y = "DDDs per 1,000 population", fill = "Care Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(here(plots_dir, "stacked_bar_regional_by_care.png"), stacked_bar_plot, width = 10, height = 6, dpi = 300)

# National DDD trends
Primary_clean <- Primary_DDD_by_year_region %>%
  mutate(year = as.integer(year)) %>%
  select(year, region, total_DDD, population, DDDs_per_1000)

Secondary_clean <- Secondary_DDD_by_year_region %>%
  mutate(year = as.integer(year)) %>%
  select(year, region, total_DDD, population, DDDs_per_1000)

Hospital_clean <- hospital_fp10_DDD_by_year_region %>%
  mutate(year = as.integer(year)) %>%
  select(year, region, total_DDD, population, DDDs_per_1000)

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
  theme_lithium(base_size = 13)
ggsave(here(plots_dir, "national_ddd_trends.png"), national_ddd_plot, width = 8, height = 5, dpi = 300)

# Regional DDD trends
Primary_clean_reg <- Primary_DDD_by_year_region %>%
  mutate(year = as.integer(year)) %>%
  select(year, region, total_DDD, population, DDDs_per_1000)
Secondary_clean_reg <- Secondary_DDD_by_year_region %>%
  mutate(year = as.integer(year)) %>%
  select(year, region, total_DDD, population, DDDs_per_1000)
Hospital_clean_reg <- hospital_fp10_DDD_by_year_region %>%
  mutate(year = as.integer(year)) %>%
  select(year, region, total_DDD, population, DDDs_per_1000)

combined_data_reg <- bind_rows(Primary_clean_reg, Secondary_clean_reg, Hospital_clean_reg)
filtered_data_reg <- combined_data_reg %>% filter(year >= 2019, year <= 2024)
summed_by_region <- filtered_data_reg %>%
  group_by(year, region) %>%
  summarise(DDDs_per_1000 = round(sum(DDDs_per_1000, na.rm = TRUE), 2), .groups = "drop") %>%
  filter(!is.na(region))

regional_trends_plot <- ggplot(summed_by_region, aes(x = year, y = DDDs_per_1000, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(x = "Year", y = "DDDs per 1,000 population", color = "Region") +
  scale_colour_nhs_region(drop = FALSE) +
  scale_y_to_next_tick(
    values = summed_by_region$DDDs_per_1000,
    labels = scales::label_number(accuracy = 0.01),
    min_upper = 300
  ) +
  scale_x_continuous(breaks = 2019:2024, expand = expansion(mult = c(0.02, 0.02))) +
  theme_lithium(base_size = 13)
ggsave(here(plots_dir, "regional_ddd_trends.png"), regional_trends_plot, width = 10, height = 6, dpi = 300)

national_ddd_summed_export <- summed_data %>%
  transmute(
    Year = year,
    `Total DDDs` = format(
      round(total_DDD_sum),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
write.csv(national_ddd_summed_export, here(data_dir, "national_DDD_summed.csv"), row.names = FALSE)
write.csv(summed_by_region, here(data_dir, "regional_DDD_trends.csv"), row.names = FALSE)

combined_regional_by_care_for_export <- combined_df_all %>%
  mutate(
    population = if_else(
      is.na(population),
      NA_character_,
      format(as.integer(round(population)), big.mark = ",", scientific = FALSE, trim = TRUE)
    ),
    total_DDD_2024 = if_else(
      is.na(total_DDD_2024),
      NA_character_,
      format(round(total_DDD_2024, 2), big.mark = ",", nsmall = 2, scientific = FALSE, trim = TRUE)
    ),
    DDDs_per_1000 = if_else(
      is.na(DDDs_per_1000),
      NA_character_,
      format(round(DDDs_per_1000, 2), big.mark = ",", nsmall = 2, scientific = FALSE, trim = TRUE)
    )
  )

write.csv(
  combined_regional_by_care_for_export,
  here(data_dir, "combined_regional_by_care_2024.csv"),
  row.names = FALSE,
  na = ""
)
combined_regional_by_care_for_export %>%
  filter(Source == "Primary") %>%
  select(-Source) %>%
  write.csv(here(data_dir, "combined_regional_by_care_2024_primary.csv"), row.names = FALSE, na = "")
combined_regional_by_care_for_export %>%
  filter(Source == "Secondary") %>%
  select(-Source) %>%
  write.csv(here(data_dir, "combined_regional_by_care_2024_secondary.csv"), row.names = FALSE, na = "")
combined_regional_by_care_for_export %>%
  filter(Source == "Hospital FP10") %>%
  select(-Source) %>%
  write.csv(here(data_dir, "combined_regional_by_care_2024_fp10.csv"), row.names = FALSE, na = "")
data.table::fwrite(
  lithium_products_DDD_summary_csv,
  here(data_dir, "lithium_products_DDD_summary.csv"),
  col.names = FALSE,
  na = ""
)
write.csv(
  lithium_DDD_summary_by_source,
  here(data_dir, "lithium_DDD_summary_by_source.csv"),
  row.names = FALSE
)
write.csv(primary_fp10_product_DDD, here(data_dir, "primary_fp10_product_DDD.csv"), row.names = FALSE)
message("Combined analysis complete. Outputs saved to ", output_dir)
