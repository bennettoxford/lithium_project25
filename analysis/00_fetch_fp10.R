library(here)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(rlang)

BASE_URL <- "https://opendata.nhsbsa.net/api/3/action"
DATASET_ID <- "hospital-prescribing-dispensed-in-the-community"
RESOURCE_PREFIX <- "HOSPITAL_DISP_COMMUNITY"
SQL_LIMIT <- 32000L
START_YEAR <- 2017L
START_MONTH <- 1L
END_YEAR <- 2025L
END_MONTH <- 12L
OUTPUT_DIR <- here("data", "secondary_care_fp10")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# BNF prefixes: Lithium Carbonate (0402030K0) or Lithium Citrate (0402030P0)
LITHIUM_PREFIXES <- c("0402030K0", "0402030P0")
LITHIUM_REGEX <- "^0402030[KP]0"

# Abbreviations used in the MONTHLY HOSPITAL DATA MMMYY (e.g. JAN24, FEB24)
MONTH_ABBR <- c(JAN = 1L, FEB = 2L, MAR = 3L, APR = 4L, MAY = 5L, JUN = 6L,
                JUL = 7L, AUG = 8L, SEP = 9L, OCT = 10L, NOV = 11L, DEC = 12L)

get_resources <- function() {
  url <- paste0(BASE_URL, "/package_show?id=", DATASET_ID)
  resp <- GET(url)
  if (http_error(resp)) stop("Failed to fetch package metadata: ", status_code(resp))
  pkg <- content(resp, as = "parsed", type = "application/json")
  if (!isTRUE(pkg$success) || is.null(pkg$result$resources)) stop("Invalid package response")

  min_yyyymm <- START_YEAR * 100L + START_MONTH
  max_yyyymm <- END_YEAR * 100L + END_MONTH
  out <- list()

  for (r in pkg$result$resources) {
    name <- r[["name"]] %||% ""

    # HOSPITAL_DISP_COMMUNITY_YYYYMM or ...YYYYMMFINAL (e.g. 202505FINAL)
    m <- regexec(paste0("^", RESOURCE_PREFIX, "_(\\d{6})(?:FINAL)?$"), name)
    if (m[[1]][1] > 0) {
      yyyymm <- as.integer(substring(name, m[[1]][2], m[[1]][2] + attr(m[[1]], "match.length")[2] - 1))
      if (yyyymm >= min_yyyymm && yyyymm <= max_yyyymm) {
        rid <- r[["id"]] %||% ""
        if (nzchar(rid)) {
          csv_url <- r[["url"]] %||% ""
          out[[length(out) + 1]] <- list(name = name, yyyymm = yyyymm, method = "datastore", resource_id = rid, csv_url = if (nzchar(csv_url)) csv_url else NULL)
        }
      }
      next
    }

    # MONTHLY HOSPITAL DATA MMMYY
    m <- regexec("^MONTHLY HOSPITAL DATA ([A-Z]{3})([0-9]{2})$", name)
    if (m[[1]][1] > 0) {
      mm <- substring(name, m[[1]][2], m[[1]][2] + attr(m[[1]], "match.length")[2] - 1)
      yy <- as.integer(substring(name, m[[1]][3], m[[1]][3] + attr(m[[1]], "match.length")[3] - 1))
      month <- MONTH_ABBR[mm]
      if (is.na(month)) next
      year <- if (yy < 50) 2000L + yy else 1900L + yy
      yyyymm <- year * 100L + as.integer(month)
      if (yyyymm >= min_yyyymm && yyyymm <= max_yyyymm) {
        url_dl <- r[["url"]] %||% ""
        if (nzchar(url_dl)) {
          out[[length(out) + 1]] <- list(name = name, yyyymm = yyyymm, method = "csv", url = url_dl)
        }
      }
    }
  }

  if (length(out) == 0) stop("No matching resources found in date range")
  out <- out[order(sapply(out, `[[`, "yyyymm"))]
  out <- lapply(out, function(r) {
    r$fallback <- if (r$method == "datastore" && !is.null(r$csv_url)) {
      list(name = paste0("CSV:", r$name), url = r$csv_url)
    } else NULL
    r
  })
  out
}

build_sql <- function(resource_id, offset = 0L) {
  likes <- paste0("BNF_CODE LIKE '", LITHIUM_PREFIXES, "%'", collapse = " OR ")
  sprintf(
    'SELECT * FROM "%s" WHERE %s LIMIT %d OFFSET %d',
    resource_id, likes, SQL_LIMIT, offset
  )
}

fetch_page <- function(resource_id, offset = 0L) {
  sql <- build_sql(resource_id, offset)
  url <- paste0(
    BASE_URL, "/datastore_search_sql?",
    "resource_id=", resource_id,
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

fetch_month_datastore <- function(resource_id) {
  all_records <- list()
  offset <- 0L

  repeat {
    page <- fetch_page(resource_id, offset)

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

fetch_month_csv <- function(download_url) {
  resp <- GET(download_url)
  if (http_error(resp)) stop("Failed to download CSV: ", status_code(resp))
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp, force = TRUE))
  writeBin(content(resp, "raw"), tmp)
  df <- read_csv(tmp, show_col_types = FALSE)
  names(df) <- gsub(" ", "_", names(df))
  if (!"BNF_CODE" %in% names(df)) stop("No BNF_CODE column in CSV")
  df <- df %>% filter(grepl(LITHIUM_REGEX, .data[["BNF_CODE"]]))
  if (nrow(df) == 0) return(NULL)
  df
}

main <- function(skip_existing = TRUE) {

  resources <- get_resources()
  n_total <- length(resources)
  message("Fetching lithium-only data for ", n_total, " months (Jan ", START_YEAR, " - Dec ", END_YEAR, ")")

  for (i in seq_along(resources)) {
    r <- resources[[i]]
    yyyymm <- r$yyyymm
    yyyymm_str <- sprintf("%06d", yyyymm)
    out_file <- file.path(OUTPUT_DIR, paste0("fp10_", yyyymm_str, ".csv"))

    if (skip_existing && file.exists(out_file)) {
      message("[", i, "/", n_total, "] Skipping ", yyyymm_str, " (already exists)")
      next
    }

    message("[", i, "/", n_total, "] Fetching ", yyyymm_str, " (", r$name, ", ", r$method, ") ...")
    df <- NULL
    err_primary <- NULL
    tryCatch({
      df <- if (r$method == "datastore") {
        fetch_month_datastore(r$resource_id)
      } else {
        fetch_month_csv(r$url)
      }
    }, error = function(e) {
      err_primary <<- e
    })
    if (is.null(df) && !is.null(err_primary) && !is.null(r$fallback)) {
      message("  Datastore failed, falling back to CSV (", r$fallback$name, ") ...")
      tryCatch({
        df <- fetch_month_csv(r$fallback$url)
      }, error = function(e) {
        message("  ERROR (primary): ", conditionMessage(err_primary))
        message("  ERROR (fallback): ", conditionMessage(e))
      })
    } else if (is.null(df) && !is.null(err_primary)) {
      message("  ERROR: ", conditionMessage(err_primary))
    }
    if (!is.null(df) && nrow(df) > 0) {
      write_csv(df, out_file)
      message("  Saved ", nrow(df), " rows to ", basename(out_file))
    } else if (is.null(df) && is.null(err_primary)) {
      message("  No lithium records for ", yyyymm_str)
    } else if (!is.null(df)) {
      message("  No lithium records for ", yyyymm_str)
    }
    Sys.sleep(0.2)
  }

  message("Done. Data saved to ", OUTPUT_DIR)
}

main(skip_existing = TRUE)
