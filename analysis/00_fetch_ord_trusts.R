library(here)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
# ORD API: https://digital.nhs.uk/developer/api-catalogue/organisation-data-service-ord
# RO197 = NHS Trust, RO24 = NHS Foundation Trust
# RE5 for ICB, RE2 for region.
BASE_URL <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations"
ROLES <- c("RO197", "RO24")
RE5 <- "RE5"
RE2 <- "RE2"
LIMIT <- 1000L
OUTPUT_DIR <- here("data")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "ord_trusts.csv")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fetch all trust ODS codes from ORD (RO197, RO24)
fetch_all_trusts_from_ord <- function() {
  all_orgs <- character()
  for (role in ROLES) {
    url <- modify_url(
      BASE_URL,
      query = list(Roles = role, Limit = LIMIT)
    )
    resp <- GET(url)
    if (http_error(resp)) {
      stop("ORD API error: ", status_code(resp), " - ", content(resp, as = "text"))
    }
    data <- content(resp, as = "parsed", type = "application/json")
    orgs <- data$Organisations
    if (!is.null(orgs) && length(orgs) > 0) {
      ids <- vapply(orgs, function(x) x$OrgId %||% NA_character_, character(1))
      all_orgs <- c(all_orgs, ids[!is.na(ids)])
    }
  }
  unique(all_orgs)
}

#' Fetch org details for a single ODS code
fetch_org_details_single <- function(org_id) {
  url <- paste0(BASE_URL, "/", org_id)
  resp <- GET(url)
  if (http_error(resp)) {
    stop("ORD API error for ", org_id, ": ", status_code(resp), " - ", content(resp, as = "text"))
  }
  content(resp, as = "parsed", type = "application/json")
}

#' Fetch details for a list of organisations
fetch_org_details <- function(orgs) {
  total <- length(orgs)
  message("Fetching details for ", total, " organisations")
  result <- list()
  for (i in seq_along(orgs)) {
    org <- orgs[i]
    result[[org]] <- fetch_org_details_single(org)
    if (i %% 50 == 0 || i == total) {
      message("Progress: ", i, "/", total, " (", round(100 * i / total, 1), "%)")
    }
    Sys.sleep(0.2)
  }
  result
}

as_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) return(list(as.list(x)))
  if (is.list(x) && !is.null(x$id)) return(list(x))
  if (is.list(x)) return(x)
  list(x)
}

#' Process org details: filter England, extract successors, predecessors, ICBs
process_org_details <- function(all_orgs_details) {
  filtered <- list()
  predecessors <- list()
  successors <- list()
  icbs <- list()

  for (org_code in names(all_orgs_details)) {
    data <- all_orgs_details[[org_code]]
    country <- data$Organisation$GeoLoc$Location$Country %||% NA_character_
    if (country != "ENGLAND") next

    filtered[[org_code]] <- data
    org <- data$Organisation

    succs <- org$Succs$Succ
    for (s in as_list(succs)) {
      type <- s$Type %||% ""
      target <- s$Target$OrgId$extension %||% s[["Target"]][["OrgId"]][["extension"]]
      if (is.null(target)) next
      if (type == "Predecessor") {
        predecessors[[org_code]] <- c(predecessors[[org_code]] %||% character(), target)
      } else if (type == "Successor") {
        successors[[org_code]] <- c(successors[[org_code]] %||% character(), target)
      }
    }

    rels <- org$Rels$Rel
    for (r in as_list(rels)) {
      if ((r$id %||% r[["id"]]) != RE5) next
      if ((r$Status %||% r[["Status"]]) != "Active") next
      icb <- r$Target$OrgId$extension %||% r[["Target"]][["OrgId"]][["extension"]]
      if (!is.null(icb)) icbs[[org_code]] <- icb
      break
    }
  }

  # RYK (Dudley) dissolved Oct 2024; staff/services transferred to TAJ (Black Country Healthcare)
  # Ref: https://www.england.nhs.uk/publication/dudley-and-walsall-mental-health-partnership-nhs-trust/
  successors[["RYK"]] <- unique(c(successors[["RYK"]] %||% character(), "TAJ"))
  predecessors[["TAJ"]] <- unique(c(predecessors[["TAJ"]] %||% character(), "RYK"))

  message("Found ", length(icbs), " trusts with ICB mapping")
  list(
    icbs = icbs,
    successors = successors,
    predecessors = predecessors,
    filtered_org_details = filtered
  )
}

#' Get region codes for ICBs (RE2 relationship)
get_icb_regions <- function(icb_codes) {
  icb_codes <- unique(icb_codes[!is.na(icb_codes) & icb_codes != ""])
  if (length(icb_codes) == 0) return(list())
  message("Fetching regions for ", length(icb_codes), " ICBs")
  icb_details <- fetch_org_details(icb_codes)
  result <- list()
  for (icb in names(icb_details)) {
    rels <- icb_details[[icb]]$Organisation$Rels$Rel
    for (r in as_list(rels)) {
      if ((r$id %||% r[["id"]]) != RE2) next
      if ((r$Status %||% r[["Status"]]) != "Active") next
      region <- r$Target$OrgId$extension %||% r[["Target"]][["OrgId"]][["extension"]]
      if (!is.null(region)) result[[icb]] <- region
      break
    }
  }
  result
}

#' Format org name (drop COMMISSIONING REGION, INTEGRATED CARE BOARD; title case)
format_org_name <- function(name) {
  if (is.null(name) || length(name) == 0 || name == "") return("")
  name <- gsub("COMMISSIONING REGION", "", name, ignore.case = TRUE)
  name <- gsub("INTEGRATED CARE BOARD", "", name, ignore.case = TRUE)
  words <- strsplit(trimws(name), "\\s+")[[1]]
  formatted <- vapply(words, function(w) {
    if (toupper(w) == "NHS") w else paste0(toupper(substr(w, 1, 1)), tolower(substr(w, 2, nchar(w))))
  }, character(1))
  out <- paste(formatted, collapse = " ")
  out <- gsub("'S ", "'s ", out)
  out <- gsub("'S,", "'s,", out)
  trimws(out)
}

#' Get names for a set of org codes
get_org_names <- function(org_codes) {
  org_codes <- unique(org_codes[!is.na(org_codes) & org_codes != ""])
  if (length(org_codes) == 0) return(list())
  message("Fetching names for ", length(org_codes), " organisations")
  details <- fetch_org_details(org_codes)
  result <- list()
  for (code in names(details)) {
    nm <- details[[code]]$Organisation$Name %||% ""
    if (nzchar(nm)) result[[code]] <- format_org_name(nm)
  }
  result
}

#' Resolve succession chains to ultimate successors
resolve_ultimate_successors <- function(successors_dict) {
  ultimate <- list()
  for (org_code in names(successors_dict)) {
    current_successors <- successors_dict[[org_code]] %||% character()
    final_successors <- character()
    visited <- character()

    for (successor in current_successors) {
      if (!successor %in% names(successors_dict) || length(successors_dict[[successor]] %||% character()) == 0) {
        final_successors <- c(final_successors, successor)
        next
      }
      current <- successor
      chain <- current
      while (current %in% names(successors_dict) && length(successors_dict[[current]] %||% character()) > 0) {
        if (current %in% visited) break
        visited <- c(visited, current)
        next_succ <- (successors_dict[[current]] %||% character())[1]
        if (next_succ %in% chain) break
        chain <- c(chain, next_succ)
        current <- next_succ
      }
      if (length(chain) > 0) final_successors <- c(final_successors, chain[length(chain)])
    }
    ultimate[[org_code]] <- unique(final_successors)
  }
  ultimate
}

create_org_mapping_df <- function(successors, predecessors, orgs_details, icbs,
                                  icb_regions, region_names, icb_names) {
  ultimate_successors <- resolve_ultimate_successors(successors)

  rows <- list()
  for (org_code in names(orgs_details)) {
    org_details <- orgs_details[[org_code]]
    org <- org_details$Organisation

    icb <- icbs[[org_code]]
    icb_name <- if (!is.null(icb)) icb_names[[icb]] %||% NA_character_ else NA_character_
    region <- if (!is.null(icb)) icb_regions[[icb]] %||% NA_character_ else NA_character_
    region_name <- if (!is.null(region)) region_names[[region]] %||% NA_character_ else NA_character_

    dates <- org$Date
    dates_dict <- list()
    if (!is.null(dates)) {
      for (d in as_list(dates)) {
        typ <- d$Type %||% d[["Type"]]
        if (!is.null(typ)) {
          dates_dict[[typ]] <- list(
            Start = d$Start %||% d[["Start"]],
            End = d$End %||% d[["End"]]
          )
        }
      }
    }

    legal <- dates_dict$Legal %||% list()
    operational <- dates_dict$Operational %||% list()

    succ_vec <- successors[[org_code]] %||% character()
    pred_vec <- predecessors[[org_code]] %||% character()
    ult_vec <- ultimate_successors[[org_code]] %||% character()

    rows[[length(rows) + 1]] <- tibble(
      ods_code = org_code,
      ods_name = format_org_name(org$Name %||% ""),
      successors = if (length(succ_vec) > 0) paste(succ_vec, collapse = ",") else NA_character_,
      predecessors = if (length(pred_vec) > 0) paste(pred_vec, collapse = ",") else NA_character_,
      ultimate_successors = if (length(ult_vec) > 0) paste(ult_vec, collapse = ",") else NA_character_,
      legal_closed_date = legal$End %||% NA_character_,
      operational_closed_date = operational$End %||% NA_character_,
      legal_open_date = legal$Start %||% NA_character_,
      operational_open_date = operational$Start %||% NA_character_,
      postcode = org$GeoLoc$Location$PostCode %||% NA_character_,
      region_code = region %||% NA_character_,
      region = region_name %||% NA_character_,
      icb_code = icb %||% NA_character_,
      icb = icb_name %||% NA_character_
    )
  }

  bind_rows(rows)
}

main <- function() {
  message("Fetching NHS Trusts (RO197, RO24) from ORD API ...")
  all_orgs <- fetch_all_trusts_from_ord()
  if (length(all_orgs) == 0) {
    message("No organisations found.")
    return(invisible(NULL))
  }
  message("Found ", length(all_orgs), " trust organisations")

  all_orgs_details <- fetch_org_details(all_orgs)
  processed <- process_org_details(all_orgs_details)

  icb_list <- unique(unlist(processed$icbs, use.names = FALSE))
  icb_regions <- get_icb_regions(icb_list)
  region_codes <- unique(unlist(icb_regions, use.names = FALSE))
  region_names <- get_org_names(region_codes)
  icb_names <- get_org_names(icb_list)

  org_df <- create_org_mapping_df(
    processed$successors,
    processed$predecessors,
    processed$filtered_org_details,
    processed$icbs,
    icb_regions,
    region_names,
    icb_names
  )

  write_csv(org_df, OUTPUT_FILE)
  message("Saved ", nrow(org_df), " organisation records to ", OUTPUT_FILE)
  invisible(org_df)
}

main()
