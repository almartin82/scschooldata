# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from SCDE.
# Data comes from two sources:
# - SC Report Cards (screportcards.com): 2018-present - comprehensive data
# - Active Student Headcounts (ed.sc.gov): 2013-present - grade and demographic data
#
# The package uses the Active Student Headcounts as the primary source because
# it provides school-level grade and demographic breakdowns.
#
# ==============================================================================

#' Download raw enrollment data from SCDE
#'
#' Downloads school and district enrollment data from SC Department of Education.
#' Uses Active Student Headcounts for grade and demographic breakdowns.
#'
#' @param end_year School year end (2023-24 = 2024)
#' @param count_day Which count day to use: "45", "135", or "180" (default: "45")
#' @return List with school_grade, school_demo, district_grade, district_demo data frames
#' @keywords internal
get_raw_enr <- function(end_year, count_day = "45") {

  # Validate year - Active Student Headcounts: 2013-2026
  if (end_year < 2013 || end_year > 2026) {
    stop("end_year must be between 2013 and 2026. Available data starts from 2012-13 school year.")
  }

  # Validate count_day
  if (!count_day %in% c("45", "135", "180")) {
    stop("count_day must be '45', '135', or '180'")
  }

  message(paste("Downloading SC enrollment data for", end_year, "(", count_day, "-day count)..."))

  # Build school year string (e.g., "2024-25")
  school_year <- sprintf("%d-%02d", end_year - 1, end_year %% 100)

  # Download school-level grade data
  message("  Downloading school headcount by grade...")
  school_grade <- download_headcount_file(end_year, school_year, "school", "grade", count_day)

  # Download school-level demographic data
  message("  Downloading school headcount by demographics...")
  school_demo <- download_headcount_file(end_year, school_year, "school", "demo", count_day)

  # Download district-level grade data
  message("  Downloading district headcount by grade...")
  district_grade <- download_headcount_file(end_year, school_year, "district", "grade", count_day)

  # Download district-level demographic data
  message("  Downloading district headcount by demographics...")
  district_demo <- download_headcount_file(end_year, school_year, "district", "demo", count_day)

  list(
    school_grade = school_grade,
    school_demo = school_demo,
    district_grade = district_grade,
    district_demo = district_demo,
    end_year = end_year
  )
}


#' Download a single headcount file from SCDE
#'
#' @param end_year School year end
#' @param school_year School year string (e.g., "2024-25")
#' @param level "school" or "district"
#' @param data_type "grade" or "demo"
#' @param count_day "45", "135", or "180"
#' @return Data frame
#' @keywords internal
download_headcount_file <- function(end_year, school_year, level, data_type, count_day) {

  # Build URL - URL patterns vary by year, so we need to try multiple patterns
  url <- build_headcount_url(end_year, school_year, level, data_type, count_day)

  # Create temp file
  tname <- tempfile(
    pattern = paste0("sc_", level, "_", data_type, "_"),
    tmpdir = tempdir(),
    fileext = ".xlsx"
  )

  # Download with httr
  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(120),
      httr::user_agent("scschooldata R package")
    )

    # Check for HTTP errors
    if (httr::http_error(response)) {
      # Try alternate URL patterns
      alt_urls <- build_alternate_urls(end_year, school_year, level, data_type, count_day)
      success <- FALSE

      for (alt_url in alt_urls) {
        response <- httr::GET(
          alt_url,
          httr::write_disk(tname, overwrite = TRUE),
          httr::timeout(120),
          httr::user_agent("scschooldata R package")
        )
        if (!httr::http_error(response)) {
          success <- TRUE
          break
        }
      }

      if (!success) {
        stop(paste("HTTP error downloading", level, data_type, "data for year", end_year))
      }
    }

    # Check file size (small files likely error pages)
    file_info <- file.info(tname)
    if (file_info$size < 1000) {
      stop(paste("Downloaded file too small, likely an error page for", level, data_type))
    }

  }, error = function(e) {
    stop(paste("Failed to download", level, data_type, "data for year", end_year,
               "\nError:", e$message))
  })

  # Read the Excel file
  df <- read_headcount_excel(tname, level, data_type)

  # Clean up temp file
  unlink(tname)

  # Add metadata
  df$end_year <- end_year

  df
}


#' Build URL for headcount file
#'
#' URL patterns have changed over the years. This function attempts to build
#' the correct URL for a given year.
#'
#' @param end_year School year end
#' @param school_year School year string
#' @param level "school" or "district"
#' @param data_type "grade" or "demo"
#' @param count_day "45", "135", or "180"
#' @return URL string
#' @keywords internal
build_headcount_url <- function(end_year, school_year, level, data_type, count_day) {

  base_url <- "https://ed.sc.gov/data/other/student-counts/active-student-headcounts"

  # Modern format (2023-24 and later)
  if (end_year >= 2024) {
    year_folder <- paste0(school_year, "-active-student-headcounts")

    if (data_type == "grade") {
      file_part <- paste0(count_day, "-day-", level, "-headcount-by-grade")
    } else {
      file_part <- paste0(count_day, "-day-", level, "-headcount-by-gender-ethnicity-and-pupils-in-poverty")
    }

    url <- paste0(base_url, "/", year_folder, "/", file_part, "/")
    return(url)
  }

  # 2021-2023 format - slightly different patterns
  if (end_year >= 2021) {
    year_folder <- paste0(school_year, "-active-student-headcounts")

    if (count_day == "45") {
      if (data_type == "grade") {
        file_part <- paste0("45-day-", level, "-headcount-by-grade-", school_year)
      } else {
        file_part <- paste0("45-day-", level, "-headcount-by-gender-ethnicity-and-pupils-in-poverty-", school_year)
      }
    } else if (count_day == "135") {
      if (data_type == "grade") {
        file_part <- paste0(level, "-headcount-by-grade")
      } else {
        file_part <- paste0(level, "-headcount-by-gender-ethnicity-and-pupils-in-poverty")
      }
    } else {
      if (data_type == "grade") {
        file_part <- paste0("180-", level, "-headcount-by-grade")
        if (level == "district") file_part <- paste0(file_part, "1")
      } else {
        file_part <- paste0("180-", level, "-headcount-by-gender-ethnicity-and-pupils-in-poverty")
      }
    }

    url <- paste0(base_url, "/", year_folder, "/", file_part, "/")
    return(url)
  }

  # Older formats (2013-2020) - more variation
  year_folder <- paste0(school_year, "-active-student-headcounts")
  if (end_year == 2017) {
    year_folder <- gsub("headcounts", "headcount", year_folder)
  }

  if (data_type == "grade") {
    file_part <- paste0(count_day, "-day-", level, "-headcount-by-grade-", school_year)
  } else {
    file_part <- paste0(count_day, "-day-", level, "-headcount-by-gender-and-ethnicity-", school_year)
  }

  url <- paste0(base_url, "/", year_folder, "/", file_part, "/")
  url
}


#' Build alternate URLs to try if primary URL fails
#'
#' @param end_year School year end
#' @param school_year School year string
#' @param level "school" or "district"
#' @param data_type "grade" or "demo"
#' @param count_day "45", "135", or "180"
#' @return Vector of alternate URLs
#' @keywords internal
build_alternate_urls <- function(end_year, school_year, level, data_type, count_day) {

  base_url <- "https://ed.sc.gov/data/other/student-counts/active-student-headcounts"
  urls <- c()

  # Try various URL patterns that have been used over the years
  year_folders <- c(
    paste0(school_year, "-active-student-headcounts"),
    paste0(school_year, "-active-student-headcount")
  )

  # Various file naming patterns
  if (data_type == "grade") {
    file_patterns <- c(
      paste0(count_day, "-day-", level, "-headcount-by-grade"),
      paste0(count_day, "thday", level, "headcountbygrade"),
      paste0(count_day, "thday", level, "-headcount-by-grade-", school_year),
      paste0(count_day, "thday", level, "headcountbygrade", gsub("-", "", school_year)),
      paste0(count_day, "-day-", level, "-headcount-by-grade-", school_year),
      paste0(count_day, "th-day-", level, "-headcount-by-grade-", school_year)
    )
  } else {
    file_patterns <- c(
      paste0(count_day, "-day-", level, "-headcount-by-gender-ethnicity-and-pupils-in-poverty"),
      paste0(count_day, "-day-", level, "-headcount-by-gender-and-ethnicity"),
      paste0(count_day, "-day-", level, "-headcount-by-gender-race"),
      paste0(count_day, "thday", level, "headcountbygenderrace"),
      paste0(count_day, "thday", level, "-by-gender-race-", school_year),
      paste0(count_day, "thday", level, "headcountbygenderrace", gsub("-", "", school_year)),
      paste0(count_day, "th-day-", level, "-headcount-by-gender-lunch-status-and-race-", school_year),
      paste0(count_day, "thday", level, "-by-gender-race-lunch-", school_year)
    )
  }

  # Also try without the trailing slash, with -xlsx suffix
  for (yf in year_folders) {
    for (fp in file_patterns) {
      urls <- c(urls, paste0(base_url, "/", yf, "/", fp, "/"))
      urls <- c(urls, paste0(base_url, "/", yf, "/", fp, "-xlsx/"))
      urls <- c(urls, paste0(base_url, "/", yf, "/", fp, "-xls/"))
    }
  }

  unique(urls)
}


#' Read headcount Excel file
#'
#' Parses the SCDE headcount Excel file which has a non-standard header structure.
#'
#' @param file_path Path to Excel file
#' @param level "school" or "district"
#' @param data_type "grade" or "demo"
#' @return Data frame with standardized column names
#' @keywords internal
read_headcount_excel <- function(file_path, level, data_type) {

  # Read raw data skipping header rows (typically 5-6 rows of metadata)
  df <- readxl::read_excel(file_path, sheet = 1, skip = 6, col_names = FALSE)

  # Get header rows to determine column names
  header_rows <- readxl::read_excel(file_path, sheet = 1, n_max = 7, col_names = FALSE)

  # Determine column names based on data type
  if (data_type == "grade") {
    col_names <- get_grade_column_names(level)
  } else {
    col_names <- get_demo_column_names(level)
  }

  # Apply column names (may need adjustment based on actual column count)
  if (ncol(df) >= length(col_names)) {
    names(df)[1:length(col_names)] <- col_names
    if (ncol(df) > length(col_names)) {
      # Extra columns get generic names
      names(df)[(length(col_names) + 1):ncol(df)] <- paste0("extra_", seq_len(ncol(df) - length(col_names)))
    }
  } else {
    # Fewer columns than expected - use what we have
    names(df) <- col_names[1:ncol(df)]
  }

  # Remove rows with NA school/district ID
  id_col <- if (level == "school") "school_id" else "district_name"
  df <- df[!is.na(df[[id_col]]) & df[[id_col]] != "", ]

  # Convert numeric columns
  numeric_cols <- setdiff(names(df), c("school_id", "district_name", "school_name"))
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- safe_numeric(df[[col]])
    }
  }

  df
}


#' Get column names for grade data files
#'
#' @param level "school" or "district"
#' @return Vector of column names
#' @keywords internal
get_grade_column_names <- function(level) {
  if (level == "school") {
    c("school_id", "district_name", "school_name", "total",
      "grade_pk", "grade_k",
      "grade_01", "grade_02", "grade_03", "grade_04",
      "grade_05", "grade_06", "grade_07", "grade_08",
      "grade_09", "grade_10", "grade_11", "grade_12")
  } else {
    c("district_name", "total",
      "grade_pk", "grade_k",
      "grade_01", "grade_02", "grade_03", "grade_04",
      "grade_05", "grade_06", "grade_07", "grade_08",
      "grade_09", "grade_10", "grade_11", "grade_12")
  }
}


#' Get column names for demographic data files
#'
#' @param level "school" or "district"
#' @return Vector of column names
#' @keywords internal
get_demo_column_names <- function(level) {
  if (level == "school") {
    c("school_id", "district_name", "school_name", "total",
      "female", "male", "gender_missing",
      "black", "native_american", "asian", "hispanic",
      "pacific_islander", "multiracial", "white",
      "econ_disadv", "not_econ_disadv")
  } else {
    c("district_name", "total",
      "female", "male", "gender_missing",
      "black", "native_american", "asian", "hispanic",
      "pacific_islander", "multiracial", "white",
      "econ_disadv", "not_econ_disadv")
  }
}


#' Download SC Report Cards data for researchers
#'
#' Alternative data source with comprehensive school information.
#' Available from 2018-present.
#'
#' @param end_year School year end (e.g., 2024 for 2023-24)
#' @return Data frame with report card data
#' @keywords internal
download_report_cards_data <- function(end_year) {

  if (end_year < 2018 || end_year > 2025) {
    stop("SC Report Cards data available from 2018-2025")
  }

  # Build URL - pattern varies slightly by year
  school_year_short <- sprintf("%02d", (end_year - 1) %% 100)
  school_year_full <- paste0((end_year - 1), "-", school_year_short)

  if (end_year >= 2024) {
    # 2024+: report-cards-data-for-researchers-2023-24
    url <- paste0(
      "https://screportcards.com/files/", end_year,
      "/data-files/report-cards-data-for-researchers-", school_year_full, "/"
    )
  } else {
    # Earlier years use "report-card" (singular)
    url <- paste0(
      "https://screportcards.com/files/", end_year,
      "/data-files/report-card-data-for-researchers-", school_year_full, "/"
    )
  }

  # Create temp file
  tname <- tempfile(pattern = "sc_reportcards_", tmpdir = tempdir(), fileext = ".xlsx")

  # Download
  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(180),
      httr::user_agent("scschooldata R package")
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

  }, error = function(e) {
    stop(paste("Failed to download Report Cards data for year", end_year,
               "\nError:", e$message))
  })

  # Read the MainPage sheet
  df <- readxl::read_excel(tname, sheet = "1.MainPage")

  # Clean up
  unlink(tname)

  # Standardize column names
  names(df) <- tolower(gsub("\\r\\n|\\s+", "_", names(df)))
  names(df) <- gsub("_+", "_", names(df))

  df
}
