library(here)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

BASE_URL <- "https://opendata.nhsbsa.net/api/3/action"
# Legacy EPD (BNF only): data until June 2025
DATASET_ID_LEGACY <- "english-prescribing-data-epd"
# EPD with SNOMED: from July 2025 onwards
# https://opendata.nhsbsa.net/dataset/english-prescribing-dataset-epd-with-snomed-code
DATASET_ID_SNOMED <- "english-prescribing-dataset-epd-with-snomed-code"
RESOURCE_PREFIX <- "EPD"

# legacy dataset ends June 2025; SNOMED dataset from July 2025
CUOFF_YYYYMM <- 202507L
SQL_LIMIT <- 32000L
START_YEAR <- 2017L
START_MONTH <- 1L
END_YEAR <- 2025L
END_MONTH <- 12L
OUTPUT_DIR <- here("data", "primary_care")
PRACTICE_CODES_FILE <- here("data", "practice_codes_ord.csv")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# BNF prefixes: Lithium Carbonate (0402030K0) or Lithium Citrate (0402030P0)
LITHIUM_PREFIXES <- c("0402030K0", "0402030P0")

load_practice_codes <- function() {
  if (!file.exists(PRACTICE_CODES_FILE)) return(NULL)
  codes <- read_csv(PRACTICE_CODES_FILE, show_col_types = FALSE)
  unique(codes$practice_code)
}

# Returns data frame with resource_id, yyyymm, is_snomed
get_resources_from_package <- function(dataset_id, min_yyyymm, max_yyyymm, is_snomed = FALSE) {
  url <- paste0(BASE_URL, "/package_show?id=", dataset_id)
  resp <- GET(url)
  if (http_error(resp)) stop("Failed to fetch package metadata: ", status_code(resp))
  pkg <- content(resp, as = "parsed", type = "application/json")
  if (!isTRUE(pkg$success) || is.null(pkg$result$resources)) {
    return(data.frame(resource_id = character(0), yyyymm = integer(0), is_snomed = logical(0)))
  }

  resources <- pkg$result$resources
  names <- vapply(resources, `[[`, character(1), "name")
  # Match EPD_YYYYMM or EPD_SNOMED_YYYYMM
  pattern <- paste0("^", RESOURCE_PREFIX, "(_SNOMED)?_(\\d{6})$")
  idx <- grep(pattern, names)
  if (length(idx) == 0) {
    return(data.frame(resource_id = character(0), yyyymm = integer(0), is_snomed = logical(0)))
  }

  yyyymm <- as.integer(sub(pattern, "\\2", names[idx]))
  keep <- yyyymm >= min_yyyymm & yyyymm <= max_yyyymm
  ord <- order(yyyymm[keep])
  data.frame(
    resource_id = names[idx][keep][ord],
    yyyymm = yyyymm[keep][ord],
    is_snomed = is_snomed,
    stringsAsFactors = FALSE
  )
}

get_resources <- function() {
  min_yyyymm <- START_YEAR * 100L + START_MONTH
  max_yyyymm <- END_YEAR * 100L + END_MONTH

  # Legacy dataset: up to June 2025 (datastore_search_sql)
  legacy_max <- min(202506L, max_yyyymm)
  legacy <- if (min_yyyymm <= legacy_max) {
    get_resources_from_package(DATASET_ID_LEGACY, min_yyyymm, legacy_max, is_snomed = FALSE)
  } else data.frame(resource_id = character(0), yyyymm = integer(0), is_snomed = logical(0))

  # SNOMED dataset: from July 2025 (datastore_search - SQL returns 500)
  snomed_min <- max(CUOFF_YYYYMM, min_yyyymm)
  snomed <- if (snomed_min <= max_yyyymm) {
    get_resources_from_package(DATASET_ID_SNOMED, snomed_min, max_yyyymm, is_snomed = TRUE)
  } else data.frame(resource_id = character(0), yyyymm = integer(0), is_snomed = logical(0))

  out <- rbind(legacy, snomed)
  if (nrow(out) == 0) stop("No matching resources found in date range")
  out
}

# --- Legacy EPD: datastore_search_sql (exact match on BNF_CHEMICAL_SUBSTANCE) ---
build_sql <- function(resource_id, offset = 0L) {
  in_list <- paste0("'", LITHIUM_PREFIXES, "'", collapse = ", ")
  sprintf(
    "SELECT * FROM `%s` WHERE BNF_CHEMICAL_SUBSTANCE IN (%s) LIMIT %d OFFSET %d",
    resource_id, in_list, SQL_LIMIT, offset
  )
}

fetch_page_legacy <- function(resource_id, offset = 0L) {
  sql <- build_sql(resource_id, offset)
  url <- paste0(
    BASE_URL, "/datastore_search_sql?",
    "resource_id=", URLencode(resource_id, reserved = TRUE),
    "&sql=", URLencode(sql)
  )
  resp <- GET(url)
  if (http_error(resp)) stop("API error: ", status_code(resp))
  out <- content(resp, as = "parsed", type = "application/json")
  if (!isTRUE(out$success)) {
    err <- if (!is.null(out$error$message)) out$error$message else "unknown"
    stop("API error: ", err)
  }
  out
}

fetch_month_legacy <- function(resource_id) {
  all_records <- list()
  offset <- 0L
  repeat {
    page <- fetch_page_legacy(resource_id, offset)
    recs <- page$result$result$records %||% page$result$records
    if (is.null(recs) || length(recs) == 0) break
    all_records <- c(all_records, recs)
    offset <- offset + length(recs)
    if (length(recs) < SQL_LIMIT) break
    Sys.sleep(0.1)
  }
  if (length(all_records) == 0) return(NULL)
  bind_rows(all_records)
}

# --- SNOMED EPD: datastore_search (SQL returns 500). Use filters for exact match. ---
fetch_page_snomed <- function(resource_id, chemical_code, offset = 0L) {
  filters <- jsonlite::toJSON(list(BNF_CHEMICAL_SUBSTANCE_CODE = chemical_code), auto_unbox = TRUE)
  url <- paste0(
    BASE_URL, "/datastore_search?",
    "resource_id=", URLencode(resource_id, reserved = TRUE),
    "&limit=", SQL_LIMIT,
    "&offset=", offset,
    "&filters=", URLencode(filters)
  )
  resp <- GET(url)
  if (http_error(resp)) stop("API error: ", status_code(resp))
  out <- content(resp, as = "parsed", type = "application/json")
  if (!isTRUE(out$success)) {
    err <- if (!is.null(out$error$message)) out$error$message else "unknown"
    stop("API error: ", err)
  }
  out
}

fetch_page_snomed_q <- function(resource_id, chemical_code, offset = 0L) {
  q <- jsonlite::toJSON(list(BNF_CHEMICAL_SUBSTANCE_CODE = chemical_code), auto_unbox = TRUE)
  url <- paste0(
    BASE_URL, "/datastore_search?",
    "resource_id=", URLencode(resource_id, reserved = TRUE),
    "&limit=", SQL_LIMIT,
    "&offset=", offset,
    "&q=", URLencode(q)
  )
  resp <- GET(url)
  if (http_error(resp)) stop("API error: ", status_code(resp))
  out <- content(resp, as = "parsed", type = "application/json")
  if (!isTRUE(out$success)) stop("API error: ", out$error$message %||% "unknown")
  out
}

fetch_month_snomed <- function(resource_id) {
  all_records <- list()
  for (chemical in LITHIUM_PREFIXES) {
    offset <- 0L
    repeat {
      page <- fetch_page_snomed(resource_id, chemical, offset)
      recs <- page$result$records
      if (is.null(recs) || length(recs) == 0) break
      all_records <- c(all_records, recs)
      offset <- offset + length(recs)
      if (length(recs) < SQL_LIMIT) break
      Sys.sleep(0.1)
    }
  }
  # If filters returned 0, fall back to q (prefix match) + exact filter
  if (length(all_records) == 0) {
    all_records <- list()
    for (chemical in LITHIUM_PREFIXES) {
      offset <- 0L
      repeat {
        page <- fetch_page_snomed_q(resource_id, chemical, offset)
        recs <- page$result$records
        if (is.null(recs) || length(recs) == 0) break
        all_records <- c(all_records, recs)
        offset <- offset + length(recs)
        if (length(recs) < SQL_LIMIT) break
        Sys.sleep(0.1)
      }
    }
  }
  if (length(all_records) == 0) return(NULL)
  df <- bind_rows(all_records)
  # Exact match: keep only lithium (filters may not work on all resources; q does prefix match)
  if ("BNF_CHEMICAL_SUBSTANCE_CODE" %in% names(df)) {
    df <- filter(df, BNF_CHEMICAL_SUBSTANCE_CODE %in% LITHIUM_PREFIXES)
  } else {
    df <- filter(df, substr(BNF_CODE, 1, 9) %in% LITHIUM_PREFIXES)
  }
  if (nrow(df) == 0) return(NULL)
  df
}

# Normalize SNOMED schema to legacy format for primary analysis compatibility
normalize_snomed_to_legacy <- function(df, yyyymm) {
  # Filter to lithium only (q param can return 0402030Q0 valproic acid, 0402030R0, etc.)
  if ("BNF_CHEMICAL_SUBSTANCE_CODE" %in% names(df)) {
    df <- filter(df, BNF_CHEMICAL_SUBSTANCE_CODE %in% LITHIUM_PREFIXES)
  } else if ("BNF_CODE" %in% names(df)) {
    df <- filter(df, substr(BNF_CODE, 1, 9) %in% LITHIUM_PREFIXES)
  } else if ("BNF_PRESENTATION_CODE" %in% names(df)) {
    df <- filter(df, substr(BNF_PRESENTATION_CODE, 1, 9) %in% LITHIUM_PREFIXES)
  }
  if (nrow(df) == 0) return(df)
  # SNOMED: YEAR_MONTH "2025-10", BNF_PRESENTATION_CODE, BNF_PRESENTATION_NAME, BNF_CHEMICAL_SUBSTANCE (name), ADQ_USAGE
  # Legacy: YEAR_MONTH "202510", BNF_CODE, BNF_DESCRIPTION, BNF_CHEMICAL_SUBSTANCE (code), ADQUSAGE
  yyyymm_str <- sprintf("%06d", yyyymm)
  df %>%
    mutate(
      YEAR_MONTH = if ("YEAR_MONTH" %in% names(.)) gsub("-", "", as.character(YEAR_MONTH)) else yyyymm_str,
      BNF_CODE = BNF_PRESENTATION_CODE,
      BNF_DESCRIPTION = BNF_PRESENTATION_NAME,
      CHEMICAL_SUBSTANCE_BNF_DESCR = BNF_CHEMICAL_SUBSTANCE,
      BNF_CHEMICAL_SUBSTANCE = BNF_CHEMICAL_SUBSTANCE_CODE,
      ADQUSAGE = if ("ADQ_USAGE" %in% names(.)) ADQ_USAGE else NA_real_
    ) %>%
    select(-any_of(c("BNF_PRESENTATION_CODE", "BNF_PRESENTATION_NAME", "BNF_CHEMICAL_SUBSTANCE_CODE", "ADQ_USAGE"))) %>%
    select(any_of(c(
      "YEAR_MONTH", "REGIONAL_OFFICE_NAME", "REGIONAL_OFFICE_CODE", "ICB_NAME", "ICB_CODE",
      "PCO_NAME", "PCO_CODE", "PRACTICE_NAME", "PRACTICE_CODE",
      "ADDRESS_1", "ADDRESS_2", "ADDRESS_3", "ADDRESS_4", "POSTCODE",
      "BNF_CHEMICAL_SUBSTANCE", "CHEMICAL_SUBSTANCE_BNF_DESCR", "BNF_CODE", "BNF_DESCRIPTION",
      "BNF_CHAPTER_PLUS_CODE", "QUANTITY", "ITEMS", "TOTAL_QUANTITY",
      "ADQUSAGE", "NIC", "ACTUAL_COST", "UNIDENTIFIED", "SNOMED_CODE"
    ))) %>%
    # SNOMED API returns duplicate rows; remove to avoid inflating totals vs legacy
    distinct()
}

`%||%` <- function(x, y) if (is.null(x)) y else x

main <- function(skip_existing = TRUE) {

  practice_codes <- load_practice_codes()
  if (is.null(practice_codes)) {
    stop("No practice_codes_ord.csv found - aborting")
  } else {
    message("Filtering to ", length(practice_codes), " ORD practices (RO177/RO76)")
  }

  resources <- get_resources()
  n_total <- nrow(resources)
  message("Fetching lithium-only EPD data for ", n_total, " months (Jan ", START_YEAR, " - Dec ", END_YEAR, ")")
  message("  Legacy EPD (BNF) up to Jun 2025; EPD with SNOMED from Jul 2025")

  for (i in seq_len(n_total)) {
    r <- resources[i, ]
    resource_id <- r$resource_id
    yyyymm <- sprintf("%06d", r$yyyymm)
    is_snomed <- r$is_snomed
    out_file <- file.path(OUTPUT_DIR, paste0("epd_lithium_", yyyymm, ".csv"))

    if (skip_existing && file.exists(out_file)) {
      message("[", i, "/", n_total, "] Skipping ", yyyymm, " (already exists)")
      next
    }

    msg_src <- if (is_snomed) "SNOMED" else "legacy"
    message("[", i, "/", n_total, "] Querying ", yyyymm, " (", msg_src, ") ...")
    tryCatch({
      df <- if (is_snomed) fetch_month_snomed(resource_id) else fetch_month_legacy(resource_id)
      if (is.null(df) || nrow(df) == 0) {
        message("  No lithium records for ", yyyymm)
        next
      }
      if (is_snomed) {
        df <- normalize_snomed_to_legacy(df, r$yyyymm)
      }
      if (!is.null(practice_codes)) {
        n_before <- nrow(df)
        df <- filter(df, PRACTICE_CODE %in% practice_codes)
        message("  Filtered ", n_before, " -> ", nrow(df), " rows (ORD practices only)")
      }
      write_csv(df, out_file)
      message("  Saved ", nrow(df), " rows to ", basename(out_file))
    }, error = function(e) {
      message("  ERROR: ", conditionMessage(e))
    })
    Sys.sleep(0.2)
  }

  message("Done. Data saved to ", OUTPUT_DIR)
}

main(skip_existing = TRUE)
