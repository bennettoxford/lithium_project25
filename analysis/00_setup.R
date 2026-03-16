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
