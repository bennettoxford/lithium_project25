output_dir <- here("output", "data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

out_csv <- file.path(output_dir, "ons_nhs_england_region_population_estimates.csv")

paths <- tibble(
  workbook_label = c(
    "ons_pop_estimates_2011-2022.xlsx",
    "ons_pop_estimates_2022-2024.xlsx"
  ),
  path = here("data", workbook_label)
)

stopifnot(all(file.exists(paths$path)))

sheet_config <- bind_rows(
  tibble(
    workbook_label = "ons_pop_estimates_2011-2022.xlsx",
    estimate_year = 2011:2021
  ),
  tibble(
    workbook_label = "ons_pop_estimates_2022-2024.xlsx",
    estimate_year = 2022:2024
  )
) %>%
  mutate(sheet = paste0("Mid-", estimate_year, " ICB 2024"))

column_config <- tibble(
  workbook_label = c(
    "ons_pop_estimates_2011-2022.xlsx",
    "ons_pop_estimates_2022-2024.xlsx"
  ),
  name_col = c("NHSER 2024 Name", "NHSER 2024 Name"),
  code_col = c("NHSER 2024 Code", "NHSER 2024 Code"),
  total_col = c("Total", "Total")
)

read_icb_year_sheet <- function(path, sheet, workbook_label, estimate_year, name_col, code_col, total_col) {
  df <- read_xlsx(path, sheet = sheet, skip = 3)

  required_cols <- c(name_col, code_col, total_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      "Missing expected columns in sheet ", sheet, " of ", workbook_label, ": ",
      paste(missing_cols, collapse = ", ")
    )
  }

  df %>%
    transmute(
      nhs_region = str_squish(as.character(.data[[name_col]])),
      nhs_region_code = as.character(.data[[code_col]]),
      population = as.numeric(.data[[total_col]])
    ) %>%
    filter(!is.na(nhs_region), nhs_region != "", !is.na(population)) %>%
    mutate(
      estimate_year = estimate_year,
      source_workbook = workbook_label,
      .before = 1
    )
}

summarise_regions <- function(path, workbook_label) {
  sheet_rows <- sheet_config %>% filter(workbook_label == .env$workbook_label)
  col_row <- column_config %>% filter(workbook_label == .env$workbook_label)

  map_dfr(seq_len(nrow(sheet_rows)), function(i) {
    read_icb_year_sheet(
      path = path,
      sheet = sheet_rows$sheet[[i]],
      workbook_label = workbook_label,
      estimate_year = sheet_rows$estimate_year[[i]],
      name_col = col_row$name_col[[1]],
      code_col = col_row$code_col[[1]],
      total_col = col_row$total_col[[1]]
    ) %>%
      group_by(estimate_year, source_workbook, nhs_region, nhs_region_code) %>%
      summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
  })
}

pop_raw <- pmap_dfr(list(paths$path, paths$workbook_label), summarise_regions)

pop_long <- pop_raw

if (nrow(pop_long) == 0L) {
  stop("No NHS England region rows were extracted")
}

expected_regions <- sort(unique(pop_long$nhs_region))
if (length(expected_regions) != 7L) {
  warning(
    "Expected 7 NHS England regions; found ", length(expected_regions), ": ",
    paste(expected_regions, collapse = ", ")
  )
}

pop_wide <- pop_long %>%
  select(estimate_year, nhs_region, nhs_region_code, population) %>%
  pivot_wider(
    id_cols = c(nhs_region, nhs_region_code),
    names_from = estimate_year,
    values_from = population,
    names_sort = TRUE,
    names_prefix = "population_"
  )

write_csv(pop_long, out_csv)

