library(here)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# ORD API: https://digital.nhs.uk/developer/api-catalogue/organisation-data-service-ord
# RO177 = primary role (GP Practice), RO76 = non-primary role (GP Practice subset of Prescribing Cost Centres)
# Hierarchy: Practice -RE4-> Sub ICB (RO98) -RE5-> ICB (RO261) -RE2-> Region (RO209)
# RE4 = IS COMMISSIONED BY (practice commissioned by Sub ICB); RE5 = IS LOCATED IN GEOGRAPHY OF (Sub ICB -> ICB); RE2 = IS A SUB-DIVISION OF (ICB -> Region)
BASE_URL <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations"
PRIMARY_ROLE <- "RO177"
NON_PRIMARY_ROLE <- "RO76"
REL_SUB_ICB <- "RE4"
REL_ICB <- "RE5"
REL_REGION <- "RE2"
RO_SUB_ICB <- "RO98"
RO_ICB <- "RO261"
RO_REGION <- "RO209"
LIMIT <- 1000L
OUTPUT_DIR <- here("data")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "practice_codes_ord.csv")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

fetch_page <- function(offset = 1L) {
  url <- modify_url(
    BASE_URL,
    query = list(
      PrimaryRoleId = PRIMARY_ROLE,
      NonPrimaryRoleId = NON_PRIMARY_ROLE,
      Limit = LIMIT,
      Offset = offset
    )
  )
  resp <- GET(url)
  if (http_error(resp)) {
    stop("ORD API error: ", status_code(resp), " - ", content(resp, as = "text"))
  }
  content(resp, as = "parsed", type = "application/json")
}

#' Extract practice codes and names from API response
parse_organisations <- function(data) {
  orgs <- data$Organisations
  if (is.null(orgs) || length(orgs) == 0) {
    return(NULL)
  }
  tibble(
    practice_code = vapply(orgs, function(x) x$OrgId %||% NA_character_, character(1)),
    name = vapply(orgs, function(x) x$Name %||% NA_character_, character(1)),
    status = vapply(orgs, function(x) x$Status %||% NA_character_, character(1))
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fetch full org details for a single ODS code
fetch_org_details <- function(org_id) {
  url <- paste0(BASE_URL, "/", org_id)
  resp <- GET(url)
  if (http_error(resp)) {
    stop("ORD API error for ", org_id, ": ", status_code(resp), " - ", content(resp, as = "text"))
  }
  content(resp, as = "parsed", type = "application/json")
}

as_rel_list <- function(rels) {
  if (is.null(rels)) return(list())
  rel <- rels$Rel
  if (is.null(rel)) return(list())
  if (is.data.frame(rel)) {
    list(rel)
  } else if (is.list(rel) && !is.null(rel$id)) {
    list(rel)
  } else {
    rel
  }
}

as_role_list <- function(roles) {
  if (is.null(roles)) return(list())
  role <- roles$Role
  if (is.null(role)) return(list())
  if (is.data.frame(role)) {
    list(role)
  } else if (is.list(role) && !is.null(role$id)) {
    list(role)
  } else {
    role
  }
}

#' TRUE if both RO177 and RO76 have Status == "Active"
has_active_ro177_and_ro76 <- function(org_data) {
  org <- org_data$Organisation
  if (is.null(org)) return(FALSE)
  roles <- org$Roles
  if (is.null(roles)) return(FALSE)
  has_ro177 <- FALSE
  has_ro76 <- FALSE
  for (r in as_role_list(roles)) {
    r_id <- if (is.list(r)) r$id else r[["id"]]
    r_status <- if (is.list(r)) r$Status else r[["Status"]]
    if (identical(r_id, PRIMARY_ROLE) && identical(r_status, "Active")) has_ro177 <- TRUE
    if (identical(r_id, NON_PRIMARY_ROLE) && identical(r_status, "Active")) has_ro76 <- TRUE
  }
  has_ro177 && has_ro76
}

#' Extract target org code from relationship matching rel_id
#' @param status Only return Active relationships (default "Active")
#' @param target_role Optional: filter by target PrimaryRoleId (e.g. RO98, RO261, RO209)
extract_rel_target <- function(org_data, rel_id, status = "Active", target_role = NULL) {
  org <- org_data$Organisation
  if (is.null(org)) return(NA_character_)
  rels <- org$Rels
  if (is.null(rels)) return(NA_character_)
  for (r in as_rel_list(rels)) {
    r_id <- if (is.list(r)) r$id else r[["id"]]
    if (!identical(r_id, rel_id)) next
    r_status <- if (is.list(r)) r$Status else r[["Status"]]
    if (!identical(r_status, status)) next
    target <- if (is.list(r)) r$Target else r[["Target"]]
    if (is.null(target)) next
    if (!is.null(target_role)) {
      pid <- target$PrimaryRoleId$id %||% target[["PrimaryRoleId"]][["id"]]
      if (!identical(pid, target_role)) next
    }
    ext <- target$OrgId$extension %||% target[["OrgId"]][["extension"]]
    return(ext %||% NA_character_)
  }
  NA_character_
}

#' Fetch all GP practices (RO177 primary, RO76 non-primary) from ORD API
fetch_ord_practices <- function() {
  all_records <- list()
  offset <- 1L

  repeat {
    message("Fetching offset ", offset, " ...")
    data <- fetch_page(offset)
    page <- parse_organisations(data)
    if (is.null(page) || nrow(page) == 0) break
    all_records <- c(all_records, list(page))
    offset <- offset + nrow(page)
    if (nrow(page) < LIMIT) break
    Sys.sleep(0.05)
  }

  if (length(all_records) == 0) {
    return(tibble(practice_code = character(), name = character(), status = character()))
  }
  bind_rows(all_records)
}

#' Walk hierarchy: practice -RE4-> Sub ICB (RO98) -RE5-> ICB (RO261) -RE2-> region (RO209)
#' GP practices are commissioned by Sub ICB (RE4, not RE5); Sub ICB has RE5 to ICB; ICB has RE2 to Region.
enrich_with_region <- function(practices) {
  n <- nrow(practices)
  icb_by_practice <- character(n)
  both_roles_active <- logical(n)
  sub_icb_to_icb <- list()
  icb_to_region <- list()

  for (i in seq_len(n)) {
    if (i %% 100 == 0 || i == n) {
      message("Fetching org details ", i, "/", n, " ...")
    }
    code <- practices$practice_code[i]
    details <- fetch_org_details(code)
    both_roles_active[i] <- has_active_ro177_and_ro76(details)
    sub_icb <- extract_rel_target(details, REL_SUB_ICB, status = "Active", target_role = RO_SUB_ICB)
    sub_icb <- sub_icb %||% NA_character_

    if (is.na(sub_icb) || sub_icb == "") {
      icb_by_practice[i] <- NA_character_
      Sys.sleep(0.05)
      next
    }

    if (is.null(sub_icb_to_icb[[sub_icb]])) {
      sub_icb_details <- fetch_org_details(sub_icb)
      icb <- extract_rel_target(sub_icb_details, REL_ICB, status = "Active", target_role = RO_ICB)
      sub_icb_to_icb[[sub_icb]] <- icb %||% NA_character_
      Sys.sleep(0.05)
    }
    icb <- sub_icb_to_icb[[sub_icb]]
    icb_by_practice[i] <- icb

    if (!is.na(icb) && icb != "" && is.null(icb_to_region[[icb]])) {
      icb_details <- fetch_org_details(icb)
      region <- extract_rel_target(icb_details, REL_REGION, status = "Active", target_role = RO_REGION)
      icb_to_region[[icb]] <- region %||% NA_character_
      Sys.sleep(0.05)
    }
    Sys.sleep(0.05)
  }

  region_codes <- unique(unlist(icb_to_region, use.names = FALSE))
  region_codes <- region_codes[!is.na(region_codes) & region_codes != ""]
  region_names <- list()
  for (rc in region_codes) {
    details <- fetch_org_details(rc)
    region_names[[rc]] <- details$Organisation$Name %||% NA_character_
    Sys.sleep(0.05)
  }

  region_code <- vapply(icb_by_practice, function(icb) icb_to_region[[icb]] %||% NA_character_, character(1))
  practices %>%
    mutate(
      icb_code = icb_by_practice,
      region_code = region_code,
      region = vapply(region_code, function(rc) region_names[[rc]] %||% NA_character_, character(1)),
      both_roles_active = both_roles_active
    )
}

main <- function() {
  message("Fetching GP practices from ORD API (PrimaryRole=RO177, NonPrimaryRole=RO76) ...")
  df <- fetch_ord_practices()
  df <- df %>% filter(!startsWith(practice_code, "W"))
  if (nrow(df) == 0) {
    message("No organisations found.")
    return(invisible(NULL))
  }
  message("Enriching with ICB and region (practice -RE4-> Sub ICB -RE5-> ICB -RE2-> region) ...")
  df <- enrich_with_region(df)
  write_csv(df, OUTPUT_FILE)
  message("Saved ", nrow(df), " practice codes with region to ", OUTPUT_FILE)
  invisible(df)
}

main()
