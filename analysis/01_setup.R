library(tidyverse)
library(scales)
library(here)
library(data.table)
library(patchwork)
library(readxl)
library(sf)
library(lubridate)

output_dir <- here("output")
plots_dir <- here("output", "plots")
data_dir <- here("output", "data")
dir.create(output_dir, showWarnings = FALSE)
dir.create(plots_dir, showWarnings = FALSE)
dir.create(data_dir, showWarnings = FALSE)

# NHS England Regions (ONS Jan 2024)
nhs_regions_file <- here("analysis", "NHS_England_Regions_January_2024_EN_BGC.geojson")
nhs_regions_sf <- st_read(nhs_regions_file, quiet = TRUE)
if (st_crs(nhs_regions_sf)$input != "EPSG:4326") {
  nhs_regions_sf <- st_transform(nhs_regions_sf, 4326)
}

region_levels_ordered <- c(
  "North East and Yorkshire",
  "North West",
  "Midlands",
  "East of England",
  "London",
  "South East",
  "South West"
)

nhser_code_to_region <- c(
  "Y56" = "London",
  "Y58" = "South West",
  "Y59" = "South East",
  "Y60" = "Midlands",
  "Y61" = "East of England",
  "Y62" = "North West",
  "Y63" = "North East and Yorkshire"
)

legacy_four_region_labels <- c(
  "North of England",
  "Midlands and East of England",
  "South of England"
)

normalise_nhs_region <- function(region) {
  cleaned <- str_squish(as.character(region))
  out <- unname(nhser_code_to_region[cleaned])
  needs_name_match <- is.na(out) & !is.na(cleaned) & cleaned != ""
  idx <- match(tolower(cleaned[needs_name_match]), tolower(region_levels_ordered))
  out[needs_name_match] <- region_levels_ordered[idx]
  out[!is.na(out) & out %in% legacy_four_region_labels] <- NA_character_
  out
}

stop_if_unmapped_regions <- function(df, id_cols, entity_label = "entity") {
  missing <- df %>% filter(is.na(.data$region))
  n_rows <- nrow(missing)
  if (n_rows == 0L) {
    return(invisible(NULL))
  }
  summary <- missing %>%
    group_by(across(all_of(id_cols))) %>%
    summarise(total_DDD = sum(.data$DDD, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(.data$total_DDD))
  stop(
    nrow(summary),
    " ",
    entity_label,
    "(s) with no mapped NHS region (",
    n_rows,
    " row(s), ",
    format(round(sum(missing$DDD, na.rm = TRUE), 1), nsmall = 1, big.mark = ","),
    " DDD total). Top by DDD:\n",
    paste(utils::capture.output(utils::head(summary, 10)), collapse = "\n"),
    "\nAdd mappings in data/geography_region_lookup.csv or related lookup files.",
    call. = FALSE
  )
}

ensure_region_column <- function(df) {
  if ("region" %in% names(df)) {
    df %>% mutate(region = normalise_nhs_region(region))
  } else if ("Region" %in% names(df)) {
    df %>%
      mutate(region = normalise_nhs_region(Region)) %>%
      select(-Region)
  } else {
    df
  }
}

code_col <- names(nhs_regions_sf)[grepl("NHSER.*CD|NHSER.*Code|^code$", names(nhs_regions_sf), ignore.case = TRUE)][1]
name_col <- names(nhs_regions_sf)[grepl("NHSER.*NM|^name$|^NAME$", names(nhs_regions_sf), ignore.case = TRUE)][1]
if (is.na(code_col)) code_col <- name_col
if (is.na(code_col)) code_col <- names(nhs_regions_sf)[!names(nhs_regions_sf) %in% c("geometry")][1]
region_from_code <- normalise_nhs_region(nhs_regions_sf[[code_col]])
region_from_name <- if (!is.na(name_col)) {
  normalise_nhs_region(nhs_regions_sf[[name_col]])
} else {
  rep(NA_character_, nrow(nhs_regions_sf))
}

nhs_regions_sf <- nhs_regions_sf %>%
  mutate(region = coalesce(region_from_code, region_from_name)) %>%
  filter(!is.na(region))

if (nrow(nhs_regions_sf) == 0) {
  stop("No NHS regions matched. ONS file columns: ", paste(names(st_read(nhs_regions_file, quiet = TRUE)), collapse = ", "))
}

population_path <- here("output", "data", "ons_nhs_england_region_population_estimates.csv")

population_annual_df <- read_csv(population_path, show_col_types = FALSE) %>%
  transmute(
    year = as.integer(estimate_year),
    region = normalise_nhs_region(nhs_region),
    population = as.numeric(population)
  ) %>%
  distinct(year, region, .keep_all = TRUE)


add_population_by_year <- function(df, year_col, region_col) {
  df %>%
    mutate(
      .population_year = as.integer(.data[[year_col]]),
      .population_region = normalise_nhs_region(.data[[region_col]])
    ) %>%
    left_join(
      population_annual_df %>%
        transmute(
          .population_year = year,
          .population_region = region,
          population
        ),
      by = c(".population_year", ".population_region")
    ) %>%
    select(-.population_year, -.population_region)
}

add_population_for_year <- function(df, region_col, population_year) {
  df %>%
    mutate(.population_region = normalise_nhs_region(.data[[region_col]])) %>%
    left_join(
      population_annual_df %>%
        filter(year == population_year) %>%
        transmute(
          .population_region = region,
          population
        ),
      by = ".population_region"
    ) %>%
    select(-.population_region)
}

colour_care_primary <- "#0072B2"
colour_care_secondary <- "#D55E00"
colour_care_fp10 <- "#009E73"

colour_care_primary_map <- c("#E8F2FA", "#4A93C9", colour_care_primary)
colour_care_secondary_map <- c("#FDEEE3", "#E7893D", colour_care_secondary)
colour_care_fp10_map <- c("#E5F5F0", "#3FA67F", colour_care_fp10)


coverage_map_value_label_size <- 4.6
coverage_map_label_halo_half_lon_deg <- 0.62
coverage_map_label_halo_half_lat_deg <- 0.11
coverage_map_london_pad_east_frac <- 0.085
coverage_map_leader_linewidth <- 0.35

coverage_map_colourbar_break_labels <- c("lower", "higher")

coverage_map_plot_margin_right <- 36

coverage_map_combined_margin_left_pt <- 100

coverage_map_legend_position <- c(0.07, 0.5)
coverage_map_legend_title_size <- 11
coverage_map_legend_text_size <- 11

colour_care_combined_aggregate <- "#333333"
axis_tick_label_size <- 14

theme_lithium <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(2.5, "mm"),
      axis.text.x = element_text(size = axis_tick_label_size),
      axis.text.y = element_text(size = axis_tick_label_size)
    )
}

theme_lithium_trend_line <- function(base_size = 13) {
  theme_lithium(base_size = base_size) +
    theme(
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
}

theme_lithium_trend_bar <- function(base_size = 13) {
  theme_lithium_trend_line(base_size = base_size) +
    theme(axis.text.x = element_text(face = "bold"))
}

theme_lithium_trend_line_legend <- function(base_size = 13) {
  theme_lithium_trend_line(base_size = base_size) +
    theme(legend.title = element_text(face = "bold"))
}

theme_lithium_coverage_map <- function() {
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
    )
}

theme_lithium_region_hist <- function() {
  theme_lithium() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = axis_tick_label_size),
      axis.text.y = element_text(size = axis_tick_label_size),
      plot.margin = margin(10, 10, 10, 10)
    )
}

coverage_map_label_layers_data <- function(
  coverage_sf,
  region_col,
  value_col = "DDDs_per_1000",
  london_pad_east_frac = coverage_map_london_pad_east_frac
) {
  if (nrow(coverage_sf) == 0) {
    return(list(
      other = tibble::tibble(lon = double(), lat = double(), label = character()),
      london_seg = tibble::tibble(
        lon = double(), lat = double(), lon_end = double(), lat_end = double()
      ),
      london_txt = tibble::tibble(lon = double(), lat = double(), label = character())
    ))
  }

  coverage_proj <- sf::st_transform(coverage_sf, 27700)
  pts <- sf::st_point_on_surface(sf::st_geometry(coverage_proj))
  pts <- sf::st_transform(pts, 4326)
  crd <- sf::st_coordinates(pts)
  nm <- coverage_sf[[region_col]]
  val <- coverage_sf[[value_col]]
  label <- ifelse(is.na(val), "", sprintf("%.2f", val))

  tfull <- tibble::tibble(
    lon = crd[, 1],
    lat = crd[, 2],
    label = label,
    is_london = nm == "London" & !is.na(nm)
  )

  other <- tfull %>%
    dplyr::filter(!is_london, label != "") %>%
    dplyr::select(lon, lat, label)
  ldn <- tfull %>% dplyr::filter(is_london, label != "")

  if (nrow(ldn) == 1L) {
    b <- sf::st_bbox(coverage_sf)
    lon_span <- as.numeric(b["xmax"] - b["xmin"])
    end_lon <- as.numeric(b["xmax"]) + london_pad_east_frac * lon_span
    end_lat <- ldn$lat
    london_seg <- tibble::tibble(
      lon = ldn$lon,
      lat = ldn$lat,
      lon_end = end_lon,
      lat_end = end_lat
    )
    london_txt <- tibble::tibble(
      lon = end_lon,
      lat = end_lat,
      label = ldn$label
    )
  } else {
    london_seg <- tibble::tibble(
      lon = double(), lat = double(), lon_end = double(), lat_end = double()
    )
    london_txt <- tibble::tibble(lon = double(), lat = double(), label = character())
  }

  list(other = other, london_seg = london_seg, london_txt = london_txt)
}


coverage_map_label_halo_rect <- function(
  pts,
  half_lon = coverage_map_label_halo_half_lon_deg,
  half_lat = coverage_map_label_halo_half_lat_deg
) {
  if (nrow(pts) == 0) {
    return(tibble::tibble(lon = double(), lat = double(), group = integer()))
  }
  gid <- seq_len(nrow(pts))
  out <- vector("list", nrow(pts))
  for (i in gid) {
    lo <- pts$lon[i]
    la <- pts$lat[i]
    out[[i]] <- tibble::tibble(
      lon = c(lo - half_lon, lo + half_lon, lo + half_lon, lo - half_lon, lo - half_lon),
      lat = c(la - half_lat, la - half_lat, la + half_lat, la + half_lat, la - half_lat),
      group = i
    )
  }
  dplyr::bind_rows(out)
}

colour_region_palette <- c(
  "North East and Yorkshire" = "#4477AA",
  "North West"               = "#EE6677",
  "Midlands"                 = "#228833",
  "East of England"          = "#CCBB44",
  "London"                   = "#66CCEE",
  "South East"               = "#AA3377",
  "South West"               = "#DDAA33"
)

scale_colour_nhs_region <- function(drop = FALSE) {
  scale_color_manual(
    values = colour_region_palette,
    limits = region_levels_ordered,
    drop = drop,
    na.value = "grey50"
  )
}

scale_y_to_next_tick <- function(values, n_breaks = 5, labels = waiver(), min_upper = NULL) {
  finite_values <- values[is.finite(values)]
  y_max <- if (length(finite_values) == 0) 0 else max(finite_values, na.rm = TRUE)
  y_max <- max(y_max, 0)

  breaks <- pretty(c(0, y_max), n = n_breaks)
  breaks <- breaks[breaks >= 0]
  if (length(breaks) < 2) {
    breaks <- c(0, if (y_max > 0) y_max else 1)
  }

  step <- breaks[2] - breaks[1]
  upper <- max(breaks)

  if (!is.null(min_upper)) {
    upper <- max(upper, ceiling(min_upper / step) * step)
  }
  if (upper <= y_max) {
    upper <- upper + step
  }

  breaks <- seq(0, upper, by = step)
  scale_y_continuous(
    limits = c(0, upper),
    breaks = breaks,
    expand = c(0, 0),
    labels = labels
  )
}

add_ddd_from_bnf_quantity <- function(df, quantity_col) {
  df %>%
    mutate(
      quantity_mg = .data[[quantity_col]] * strnt_nmrtr_val,
      mmol = case_when(
        chemical == "Lithium Carbonate" ~ quantity_mg / 37.04,
        chemical == "Lithium Citrate" ~ quantity_mg / 94.26,
        TRUE ~ NA_real_
      ),
      DDD = mmol / 24
    )
}

format_ddd_by_year_for_export <- function(df, year_col, ddd_col = "total_DDD") {
  df %>%
    transmute(
      Year = as.integer(.data[[year_col]]),
      `Total DDDs` = format(
        round(.data[[ddd_col]]),
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    )
}

read_ddd_by_year_export_csv <- function(path) {
  raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  y <- as.integer(gsub(",", "", trimws(as.character(raw[["Year"]]))))
  ddd <- as.numeric(gsub(",", "", trimws(as.character(raw[["Total DDDs"]]))))
  tibble(
    year = as.character(y),
    total_DDD = ddd
  )
}

read_monthly_csvs <- function(dir, file_pattern, select_cols = NULL) {
  files <- sort(list.files(dir, pattern = file_pattern, full.names = TRUE))
  if (length(files) == 0) {
    stop("No files matching ", file_pattern, " in ", dir)
  }
  rbindlist(
    lapply(files, function(f) {
      if (is.null(select_cols)) {
        fread(f)
      } else {
        fread(f, select = select_cols)
      }
    }),
    use.names = TRUE,
    fill = TRUE
  ) %>%
    as_tibble()
}

normalize_epd_geography <- function(df) {
  if (!"ICB_CODE" %in% names(df) && "STP_CODE" %in% names(df)) {
    df[["ICB_CODE"]] <- df[["STP_CODE"]]
  }
  if (!"ICB_NAME" %in% names(df) && "STP_NAME" %in% names(df)) {
    df[["ICB_NAME"]] <- df[["STP_NAME"]]
  }
  df
}

load_epd_lithium <- function() {
  files <- sort(list.files(
    here("data", "primary_care"),
    pattern = "^epd_lithium_\\d{6}\\.csv$",
    full.names = TRUE
  ))
  if (length(files) == 0) {
    stop("No epd_lithium_YYYYMM.csv files found in data/primary_care")
  }

  cols_needed <- c(
    "YEAR_MONTH", "REGIONAL_OFFICE_CODE", "REGIONAL_OFFICE_NAME", "PCO_CODE", "ICB_CODE",
    "PRACTICE_CODE", "BNF_CODE", "BNF_DESCRIPTION",
    "TOTAL_QUANTITY"
  )

  rbindlist(
    lapply(files, function(f) {
      df <- normalize_epd_geography(fread(f))
      missing_cols <- setdiff(cols_needed, names(df))
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          df[[col]] <- NA
        }
      }
      df[, ..cols_needed]
    }),
    use.names = TRUE,
    fill = TRUE
  ) %>%
    mutate(
      YEAR_MONTH = as.character(YEAR_MONTH),
      across(
        c(
          REGIONAL_OFFICE_CODE, REGIONAL_OFFICE_NAME, PCO_CODE, ICB_CODE, PRACTICE_CODE,
          BNF_CODE, BNF_DESCRIPTION
        ),
        ~ trimws(as.character(.x))
      ),
      TOTAL_QUANTITY = as.numeric(TOTAL_QUANTITY)
    )
}

ORD_RO76_PRACTICES_FILE <- here("output", "data", "ord_ro76_practices.csv")
PRACTICE_RO76_PERIODS_FILE <- here("output", "data", "ord_ro76_practice_periods.csv")

coerce_operational_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  x <- as.character(x)
  x[x == ""] <- NA_character_
  as.Date(x)
}

load_ord_practice_periods <- function() {
  if (!file.exists(PRACTICE_RO76_PERIODS_FILE)) {
    stop(
      "ord_ro76_practice_periods.csv not found at ", PRACTICE_RO76_PERIODS_FILE, ". "
    )
  }
  read_csv(PRACTICE_RO76_PERIODS_FILE, show_col_types = FALSE) %>%
    transmute(
      practice_code = practice_code,
      period_id = period_id,
      status = role_status,
      active_from = coerce_operational_date(operational_start),
      active_to = coerce_operational_date(operational_end)
    )
}

load_geography_region_lookup <- function() {
  lookup_file <- here("data", "geography_region_lookup.csv")
  if (!file.exists(lookup_file)) {
    stop(
      "geography_region_lookup.csv not found at ", lookup_file, ". ",
      "Edit data/geography_region_lookup.csv to map ICB and sub-ICB codes to regions."
    )
  }
  read_csv(lookup_file, show_col_types = FALSE) %>%
    transmute(
      geography_code = geography_code,
      code_type = code_type,
      region = normalise_nhs_region(region)
    ) %>%
    filter(
      !is.na(geography_code),
      geography_code != "",
      !is.na(code_type),
      code_type != "",
      !is.na(region)
    ) %>%
    distinct(geography_code, code_type, .keep_all = TRUE)
}

load_ord_practice_regions <- function() {
  if (!file.exists(ORD_RO76_PRACTICES_FILE)) {
    stop(
      "ord_ro76_practices.csv not found at ", ORD_RO76_PRACTICES_FILE, ". "
    )
  }

  geography_lookup <- load_geography_region_lookup()
  icb_lookup <- geography_lookup %>%
    filter(code_type == "icb") %>%
    transmute(region_code = geography_code, region_from_icb = region)
  sub_icb_lookup <- geography_lookup %>%
    filter(code_type == "sub_icb") %>%
    transmute(sub_icb_code = geography_code, region_from_sub_icb = region)
  practice_lookup <- geography_lookup %>%
    filter(code_type == "practice") %>%
    transmute(practice_code = geography_code, region_from_practice = region)

  read_csv(ORD_RO76_PRACTICES_FILE, show_col_types = FALSE) %>%
    left_join(icb_lookup, by = "region_code") %>%
    left_join(sub_icb_lookup, by = "sub_icb_code") %>%
    left_join(practice_lookup, by = "practice_code") %>%
    transmute(
      practice_code = practice_code,
      region = coalesce(
        normalise_nhs_region(region),
        region_from_icb,
        region_from_sub_icb,
        region_from_practice
      )
    ) %>%
    group_by(practice_code) %>%
    summarise(
      region = if (all(is.na(region))) NA_character_ else first(region[!is.na(region)]),
      .groups = "drop"
    )
}

#' Keep prescribing rows for in-scope practices in any RO76 active month
filter_prescribing_by_practice_activity <- function(df, practice_periods) {
  practice_codes <- unique(practice_periods$practice_code)
  active_month_practice <- df %>%
    distinct(month, practice) %>%
    inner_join(practice_periods, by = c("practice" = "practice_code"), relationship = "many-to-many") %>%
    filter(
      is.na(active_from) | month >= floor_date(active_from, "month"),
      is.na(active_to) | month <= floor_date(active_to, "month")
    ) %>%
    distinct(month, practice)

  df %>%
    filter(practice %in% practice_codes) %>%
    semi_join(active_month_practice, by = c("month", "practice"))
}

load_fp10_monthly <- function() {
  read_monthly_csvs(
    here("data", "secondary_care_fp10"),
    file_pattern = "^fp10_\\d{6}\\.csv$",
    select_cols = c(
      "PERIOD", "BNF_CODE", "BNF_NAME",
      "HOSPITAL_TRUST_CODE", "HOSPITAL_TRUST",
      "TOTAL_QUANTITY"
    )
  ) %>%
    mutate(
      PERIOD = as.Date(paste0(as.character(PERIOD), "01"), format = "%Y%m%d"),
      across(
        c(BNF_CODE, BNF_NAME, HOSPITAL_TRUST_CODE, HOSPITAL_TRUST),
        ~ trimws(as.character(.x))
      ),
      TOTAL_QUANTITY = as.numeric(TOTAL_QUANTITY)
    ) %>%
    filter(!is.na(PERIOD))
}

load_ord_trust_region_mapping <- function() {
  read_csv(here("data", "ord_trusts.csv"), show_col_types = FALSE) %>%
    filter(!is.na(region_code), region_code != "") %>%
    mutate(trust_code_prefix = substr(ods_code, 1, 3)) %>%
    transmute(
      trust_code_prefix,
      region_from_ord = normalise_nhs_region(region_code)
    ) %>%
    distinct(trust_code_prefix, .keep_all = TRUE)
}

load_trust_region_mapping <- function() {
  trust_overrides <- load_geography_region_lookup() %>%
    filter(code_type == "trust") %>%
    transmute(
      trust_code_prefix = geography_code,
      region_from_lookup = region
    )

  load_ord_trust_region_mapping() %>%
    full_join(trust_overrides, by = "trust_code_prefix") %>%
    transmute(
      trust_code_prefix,
      region = coalesce(region_from_lookup, region_from_ord)
    ) %>%
    filter(!is.na(region)) %>%
    distinct(trust_code_prefix, .keep_all = TRUE)
}
