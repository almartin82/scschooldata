# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading school directory data from the
# South Carolina Department of Education (SCDE) via the SC Report Cards
# "Additional Info" data files.
#
# Data source: https://screportcards.com/files/{year}/data-files/
# Sheet: 1.MainPage (contains school/district addresses, principals,
# superintendents, grade spans, phone numbers, and websites)
#
# Available years: 2018-2025
#
# ==============================================================================

#' Fetch South Carolina school directory data
#'
#' Downloads and processes school directory data from the SC Report Cards
#' website. This includes school and district contact information, principal
#' and superintendent names, addresses, phone numbers, grade spans, and
#' school websites.
#'
#' @param end_year School year end (e.g., 2025 for 2024-25). Valid: 2018-2025.
#'   If NULL (default), uses the most recent available year.
#' @param tidy If TRUE (default), returns data in a standardized format with
#'   consistent column names. If FALSE, returns raw column names from SCDE.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from SCDE.
#' @return A tibble with school directory data. Tidy columns include:
#'   \itemize{
#'     \item \code{end_year}: School year end
#'     \item \code{state_district_id}: District portion of SCHOOLID (first 3 digits)
#'     \item \code{state_school_id}: Full 7-digit SCHOOLID
#'     \item \code{district_name}: District name
#'     \item \code{school_name}: School name
#'     \item \code{entity_type}: Entity type (School or District)
#'     \item \code{school_type}: Report card type (P=Primary, E=Elementary, M=Middle, H=High, D=District, S=Special)
#'     \item \code{address}: Street address
#'     \item \code{city}: City
#'     \item \code{state}: State (always "SC")
#'     \item \code{zip}: ZIP code
#'     \item \code{phone}: Phone number
#'     \item \code{grades_served}: Grade range (e.g., "9-12")
#'     \item \code{county_name}: County (NA -- not available in this data source)
#'     \item \code{superintendent_name}: District superintendent name (district rows only)
#'     \item \code{superintendent_email}: Superintendent email (NA -- not available)
#'     \item \code{principal_name}: School principal name (school rows only)
#'     \item \code{principal_email}: Principal email (NA -- not available)
#'     \item \code{website}: School or district website URL
#'     \item \code{enrollment}: Enrollment count
#'     \item \code{teacher_count}: Number of teachers
#'     \item \code{board_chair}: School board chair name (district rows only)
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Get latest directory data
#' dir_data <- fetch_directory()
#'
#' # Get specific year
#' dir_2024 <- fetch_directory(2024)
#'
#' # Get raw format (original SCDE column names)
#' dir_raw <- fetch_directory(tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' dir_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Filter to specific district
#' library(dplyr)
#' greenville <- dir_data |>
#'   filter(grepl("Greenville", district_name))
#' }
fetch_directory <- function(end_year = NULL, tidy = TRUE, use_cache = TRUE) {

  # Default to most recent year
  if (is.null(end_year)) {
    end_year <- 2025
  }

  # Validate year
  if (end_year < 2018 || end_year > 2025) {
    stop(paste0(
      "end_year must be between 2018 and 2025.\n",
      "SC Report Cards directory data is available from 2017-18 school year."
    ))
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) {
    paste0("directory_tidy_", end_year)
  } else {
    paste0("directory_raw_", end_year)
  }

  # Check cache first
  if (use_cache && cache_exists_directory(cache_type)) {
    message(paste("Using cached directory data for", end_year))
    return(read_cache_directory(cache_type))
  }

  # Get raw data from SCDE
  raw <- get_raw_directory(end_year)

  # Process to standard schema
  if (tidy) {
    result <- process_directory(raw, end_year)
  } else {
    result <- raw
  }

  # Cache the result
  if (use_cache) {
    write_cache_directory(result, cache_type)
  }

  result
}


#' Get raw school directory data from SCDE
#'
#' Downloads the raw school directory Excel file from the SC Report Cards
#' website. The data comes from the "Additional Info" data file, sheet
#' "1.MainPage".
#'
#' @param end_year School year end (e.g., 2025 for 2024-25)
#' @return Raw data frame from the 1.MainPage sheet
#' @keywords internal
get_raw_directory <- function(end_year) {

  # Build candidate URLs
  urls <- build_directory_urls(end_year)

  message(paste("Downloading SC school directory data for", end_year, "..."))

  # Create temp file
  tname <- tempfile(
    pattern = "sc_directory_",
    tmpdir = tempdir(),
    fileext = ".xlsx"
  )

  # Try each URL pattern until one succeeds
  success <- FALSE
  last_error <- NULL

  for (url in urls) {
    result <- tryCatch({
      response <- httr::GET(
        url,
        httr::write_disk(tname, overwrite = TRUE),
        httr::timeout(180),
        httr::user_agent("scschooldata R package"),
        httr::config(ssl_verifypeer = FALSE)
      )

      if (!httr::http_error(response) && file.info(tname)$size > 10000) {
        TRUE
      } else {
        FALSE
      }
    }, error = function(e) {
      last_error <<- e$message
      FALSE
    })

    if (isTRUE(result)) {
      success <- TRUE
      message(paste("  Downloaded", round(file.info(tname)$size / 1024, 1), "KB"))
      break
    }
  }

  if (!success) {
    unlink(tname)
    stop(paste(
      "Failed to download directory data for year", end_year,
      "\nTried", length(urls), "URL patterns, all returned errors.",
      "\nLast error:", if (!is.null(last_error)) last_error else "HTTP error"
    ))
  }

  # Read the 1.MainPage sheet
  df <- tryCatch(
    readxl::read_excel(
      tname,
      sheet = "1.MainPage",
      col_types = "text"
    ),
    error = function(e) {
      stop(paste(
        "Failed to read 1.MainPage sheet from downloaded file.",
        "\nError:", e$message,
        "\nThe file format may have changed."
      ))
    }
  )

  # Clean up temp file
  unlink(tname)

  message(paste("  Loaded", nrow(df), "records"))

  # Convert to tibble for consistency
  dplyr::as_tibble(df)
}


#' Build candidate URLs for SC Report Cards additional info download
#'
#' SC DOE has changed URL patterns multiple times. This function generates
#' all known patterns for a given year so the download function can try them.
#'
#' @param end_year School year end (e.g., 2025 for 2024-25)
#' @return Vector of candidate URLs to try
#' @keywords internal
build_directory_urls <- function(end_year) {

  short_yr <- sprintf("%02d", end_year %% 100)
  prev_yr <- end_year - 1
  school_year_short <- paste0(prev_yr, "-", short_yr)
  school_year_long <- paste0(prev_yr, "-", end_year)

  base <- paste0("https://screportcards.com/files/", end_year, "/data-files/")

  # URL patterns have varied by year:
  # 2023-2025: report-cards-data-additional-info-for-{prev_yr}-{short_yr}
  # 2022:      report-cards-data-additional-info-for-{prev_yr}-{end_year}
  # 2020-2021: report-cards-data-additional-info-{prev_yr}-{end_year}
  # 2018-2019: report-card-data-additional-info-{prev_yr}-{end_year}
  urls <- c(
    # 2023+ pattern (plural "report-cards", "for", short year)
    paste0(base, "report-cards-data-additional-info-for-", school_year_short, "/"),
    paste0(base, "report-cards-data-additional-info-for-", school_year_short),
    # 2022 pattern (plural, "for", long year)
    paste0(base, "report-cards-data-additional-info-for-", school_year_long, "/"),
    paste0(base, "report-cards-data-additional-info-for-", school_year_long),
    # 2020-2021 pattern (plural, no "for", long year)
    paste0(base, "report-cards-data-additional-info-", school_year_long, "/"),
    paste0(base, "report-cards-data-additional-info-", school_year_long),
    # 2018-2019 pattern (singular "report-card", no "for", long year)
    paste0(base, "report-card-data-additional-info-", school_year_long, "/"),
    paste0(base, "report-card-data-additional-info-", school_year_long),
    # Additional variations: singular with short year
    paste0(base, "report-card-data-additional-info-for-", school_year_short, "/"),
    paste0(base, "report-card-data-additional-info-for-", school_year_short),
    # Singular with "for" and long year
    paste0(base, "report-card-data-additional-info-for-", school_year_long, "/"),
    paste0(base, "report-card-data-additional-info-for-", school_year_long),
    # Plural, no "for", short year
    paste0(base, "report-cards-data-additional-info-", school_year_short, "/"),
    paste0(base, "report-cards-data-additional-info-", school_year_short)
  )

  unique(urls)
}


#' Process raw SC directory data to standard schema
#'
#' Takes raw directory data from the SC Report Cards 1.MainPage sheet and
#' standardizes column names and types.
#'
#' @param raw_data Raw data frame from get_raw_directory()
#' @param end_year School year end
#' @return Processed tibble with standard schema
#' @keywords internal
process_directory <- function(raw_data, end_year) {

  cols <- names(raw_data)

  # Standardize column names for matching (SC uses varying case/formatting)
  cols_lower <- tolower(gsub("[\\r\\n\\s]+", "_", cols))

  # Helper to find columns with flexible matching
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  n_rows <- nrow(raw_data)
  result <- dplyr::tibble(.rows = n_rows)

  # End year
  result$end_year <- as.integer(end_year)

  # School ID (7-digit SIDN)
  school_id_col <- find_col(c("^SCHOOLID$", "^School.?ID$", "^SIDN$"))
  if (!is.null(school_id_col)) {
    result$state_school_id <- trimws(as.character(raw_data[[school_id_col]]))
    # District ID is first 3 digits of 7-digit school ID
    result$state_district_id <- substr(result$state_school_id, 1, 3)
  } else {
    result$state_school_id <- NA_character_
    result$state_district_id <- NA_character_
  }

  # District name
  dist_name_col <- find_col(c("^DistrictNm$", "^District.?Nm$", "^District.?Name$"))
  if (!is.null(dist_name_col)) {
    result$district_name <- clean_text(as.character(raw_data[[dist_name_col]]))
  } else {
    result$district_name <- NA_character_
  }

  # School name
  school_name_col <- find_col(c("^SchoolNm$", "^School.?Nm$", "^School.?Name$"))
  if (!is.null(school_name_col)) {
    result$school_name <- clean_text(as.character(raw_data[[school_name_col]]))
  } else {
    result$school_name <- NA_character_
  }

  # Report card type -> entity_type and school_type
  type_col <- find_col(c("Report Card", "ReportCardType", "report_card"))
  if (!is.null(type_col)) {
    raw_type <- trimws(as.character(raw_data[[type_col]]))
    result$entity_type <- ifelse(raw_type == "D", "District", "School")
    result$school_type <- raw_type
  } else {
    result$entity_type <- NA_character_
    result$school_type <- NA_character_
  }

  # Address
  street_col <- find_col(c("^street$", "^Street$", "^Address$"))
  if (!is.null(street_col)) {
    result$address <- trimws(as.character(raw_data[[street_col]]))
  } else {
    result$address <- NA_character_
  }

  # City
  city_col <- find_col(c("^city$", "^City$"))
  if (!is.null(city_col)) {
    result$city <- trimws(as.character(raw_data[[city_col]]))
  } else {
    result$city <- NA_character_
  }

  # State (always SC)
  state_col <- find_col(c("^state$", "^State$"))
  if (!is.null(state_col)) {
    result$state <- trimws(as.character(raw_data[[state_col]]))
  } else {
    result$state <- "SC"
  }

  # ZIP
  zip_col <- find_col(c("^zip$", "^Zip$", "^ZIP$"))
  if (!is.null(zip_col)) {
    result$zip <- trimws(as.character(raw_data[[zip_col]]))
  } else {
    result$zip <- NA_character_
  }

  # Phone
  phone_col <- find_col(c("^hdphone$", "^phone$", "^Phone$"))
  if (!is.null(phone_col)) {
    result$phone <- trimws(as.character(raw_data[[phone_col]]))
  } else {
    result$phone <- NA_character_
  }

  # Grade span
  grade_col <- find_col(c("^GRADESPAN$", "^Grade.?Span$", "^Grades$"))
  if (!is.null(grade_col)) {
    result$grades_served <- trimws(as.character(raw_data[[grade_col]]))
  } else {
    result$grades_served <- NA_character_
  }

  # County (not available in this data source)
  result$county_name <- NA_character_

  # Superintendent name (district rows only)
  super_col <- find_col(c("^supername$", "^Superintendent$", "^SuperintendentName$"))
  if (!is.null(super_col)) {
    result$superintendent_name <- clean_text(as.character(raw_data[[super_col]]))
    # Replace "NA" strings with actual NA
    result$superintendent_name[result$superintendent_name == "NA"] <- NA_character_
  } else {
    result$superintendent_name <- NA_character_
  }

  # Superintendent email (not available in this data source)
  result$superintendent_email <- NA_character_

  # Principal/head name (school rows only)
  head_col <- find_col(c("^headname$", "^Principal$", "^PrincipalName$"))
  if (!is.null(head_col)) {
    result$principal_name <- clean_text(as.character(raw_data[[head_col]]))
    result$principal_name[result$principal_name == "NA"] <- NA_character_
  } else {
    result$principal_name <- NA_character_
  }

  # Principal email (not available in this data source)
  result$principal_email <- NA_character_

  # Website
  url_col <- find_col(c("^URL$", "^Website$", "^Web$"))
  if (!is.null(url_col)) {
    result$website <- trimws(as.character(raw_data[[url_col]]))
    result$website[result$website == "NA"] <- NA_character_
  } else {
    result$website <- NA_character_
  }

  # Enrollment
  enr_col <- find_col(c("^Enrollment$", "^enrollment$"))
  if (!is.null(enr_col)) {
    result$enrollment <- safe_numeric(raw_data[[enr_col]])
  } else {
    result$enrollment <- NA_integer_
  }

  # Teacher count
  teacher_col <- find_col(c("^TeacherCount$", "^Teacher.?Count$"))
  if (!is.null(teacher_col)) {
    result$teacher_count <- safe_numeric(raw_data[[teacher_col]])
  } else {
    result$teacher_count <- NA_integer_
  }

  # Board chair (district rows only)
  chair_col <- find_col(c("^chair$", "^Chair$", "^BoardChair$"))
  if (!is.null(chair_col)) {
    result$board_chair <- clean_text(as.character(raw_data[[chair_col]]))
    result$board_chair[result$board_chair == "NA"] <- NA_character_
  } else {
    result$board_chair <- NA_character_
  }

  result
}


# ==============================================================================
# Directory-specific cache functions
# ==============================================================================

#' Build cache file path for directory data
#'
#' @param cache_type Type of cache (e.g., "directory_tidy_2025")
#' @return File path string
#' @keywords internal
build_cache_path_directory <- function(cache_type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(cache_type, ".rds"))
}


#' Check if cached directory data exists
#'
#' @param cache_type Type of cache
#' @param max_age Maximum age in days (default 7)
#' @return Logical indicating if valid cache exists
#' @keywords internal
cache_exists_directory <- function(cache_type, max_age = 7) {
  cache_path <- build_cache_path_directory(cache_type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param cache_type Type of cache
#' @return Cached data frame
#' @keywords internal
read_cache_directory <- function(cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param data Data frame to cache
#' @param cache_type Type of cache
#' @return Invisibly returns the cache path
#' @keywords internal
write_cache_directory <- function(data, cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  cache_dir <- dirname(cache_path)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  saveRDS(data, cache_path)
  invisible(cache_path)
}


#' Clear school directory cache
#'
#' Removes cached school directory data files.
#'
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear cached directory data
#' clear_directory_cache()
#' }
clear_directory_cache <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist")
    return(invisible(0))
  }

  files <- list.files(cache_dir, pattern = "^directory_", full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}
