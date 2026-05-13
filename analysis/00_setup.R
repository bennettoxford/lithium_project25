library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(here)
library(readr)
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

nhser_to_region <- c(
  "Y56" = "London", "Y58" = "South West", "Y59" = "South East",
  "Y60" = "Midlands", "Y61" = "East Of England", "Y62" = "North West",
  "Y63" = "North East And Yorkshire",
  "London" = "London", "South West" = "South West", "South East" = "South East",
  "Midlands" = "Midlands", "East of England" = "East Of England",
  "North West" = "North West", "North East and Yorkshire" = "North East And Yorkshire"
)
nhser_to_Region <- c(
  "Y56" = "London", "Y58" = "South West", "Y59" = "South East",
  "Y60" = "Midlands", "Y61" = "East of England", "Y62" = "North West",
  "Y63" = "North East And Yorkshire",
  "London" = "London", "South West" = "South West", "South East" = "South East",
  "Midlands" = "Midlands", "East of England" = "East of England",
  "North West" = "North West", "North East and Yorkshire" = "North East And Yorkshire"
)

code_col <- names(nhs_regions_sf)[grepl("NHSER.*CD|NHSER.*Code|^code$", names(nhs_regions_sf), ignore.case = TRUE)][1]
name_col <- names(nhs_regions_sf)[grepl("NHSER.*NM|^name$|^NAME$", names(nhs_regions_sf), ignore.case = TRUE)][1]
if (is.na(code_col)) code_col <- name_col
if (is.na(code_col)) code_col <- names(nhs_regions_sf)[!names(nhs_regions_sf) %in% c("geometry")][1]
region_from_code <- nhser_to_region[as.character(nhs_regions_sf[[code_col]])]
region_from_name <- if (!is.na(name_col)) nhser_to_region[as.character(nhs_regions_sf[[name_col]])] else rep(NA_character_, nrow(nhs_regions_sf))
Region_from_code <- nhser_to_Region[as.character(nhs_regions_sf[[code_col]])]
Region_from_name <- if (!is.na(name_col)) nhser_to_Region[as.character(nhs_regions_sf[[name_col]])] else rep(NA_character_, nrow(nhs_regions_sf))

nhs_regions_sf <- nhs_regions_sf %>%
  mutate(
    region = coalesce(region_from_code, region_from_name),
    Region = coalesce(Region_from_code, Region_from_name)
  ) %>%
  filter(!is.na(region))

if (nrow(nhs_regions_sf) == 0) {
  stop("No NHS regions matched. ONS file columns: ", paste(names(st_read(nhs_regions_file, quiet = TRUE)), collapse = ", "))
}

# ONS mid-2023 population estimates (7 regions)
population_df <- tibble(
  region = c("North East And Yorkshire", "North West", "Midlands", "East Of England",
             "London", "South East", "South West"),
  Region = c("North East And Yorkshire", "North West", "Midlands", "East of England",
            "London", "South East", "South West"),
  population = c(8220282, 7515718, 10951858, 6401418, 8869043, 9387286, 5766937)
)

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

  pts <- sf::st_point_on_surface(sf::st_geometry(coverage_sf))
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

standardise_region <- function(region) {
  region <- tolower(region)
  region <- trimws(region)
  region <- gsub(" of ", " Of ", region)
  region <- tools::toTitleCase(region)
  region
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

colour_region_palette <- c(
  "North East And Yorkshire" = "#4477AA",
  "North East and Yorkshire" = "#4477AA",
  "North West"               = "#EE6677",
  "Midlands"                 = "#228833",
  "East Of England"          = "#CCBB44",
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
