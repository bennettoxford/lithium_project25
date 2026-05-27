source(here::here("analysis", "00_setup.R"))

unique_product_pairs <- function(df, code_col, name_col) {
  df %>%
    transmute(
      product_code = trimws(as.character(.data[[code_col]])),
      product_name = trimws(as.character(.data[[name_col]]))
    ) %>%
    filter(
      !is.na(product_code), product_code != "",
      !is.na(product_name), product_name != ""
    ) %>%
    distinct(product_code, product_name) %>%
    arrange(product_code, product_name)
}

# --- Primary care ---
primary_lithium_raw <- load_epd_lithium() %>%
  select(BNF_CODE, BNF_DESCRIPTION)

products_primary_care <- unique_product_pairs(
  primary_lithium_raw,
  code_col = "BNF_CODE",
  name_col = "BNF_DESCRIPTION"
)

# --- Secondary care ---
secondary_care <- read_csv(
  here("data", "secondary_care", "secondary_care.csv"),
  show_col_types = FALSE,
  col_types = cols(`VMP Code` = col_character())
)

products_secondary_care <- unique_product_pairs(
  secondary_care,
  code_col = "VMP Code",
  name_col = "VMP Name"
)

# --- Hospital FP10 ---
hospital_fp10_raw <- load_fp10_monthly() %>%
  select(BNF_CODE, BNF_NAME)

products_hospital_fp10 <- unique_product_pairs(
  hospital_fp10_raw,
  code_col = "BNF_CODE",
  name_col = "BNF_NAME"
)

product_tables <- list(
  "unique_products_primary_care.csv" = products_primary_care,
  "unique_products_secondary_care.csv" = products_secondary_care,
  "unique_products_hospital_fp10.csv" = products_hospital_fp10
)

iwalk(product_tables, ~ write.csv(.x, here(data_dir, .y), row.names = FALSE))
