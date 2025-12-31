# ==============================================================================
# Enrollment Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading enrollment data from the
# South Carolina Department of Education (SCDE) website.
#
# ==============================================================================

#' Fetch South Carolina enrollment data
#'
#' Downloads and processes enrollment data from the SC Department of Education
#' Active Student Headcounts.
#'
#' @param end_year A school year end. Year is the end of the academic year - eg 2023-24
#'   school year is year '2024'. Valid values are 2013-2026.
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from SCDE.
#' @param count_day Which count day to use: "45" (default), "135", or "180".
#'   The 45-day count is typically taken in October/November.
#' @return Data frame with enrollment data. Wide format includes columns for
#'   district_id, campus_id, names, and enrollment counts by demographic/grade.
#'   Tidy format pivots these counts into subgroup and grade_level columns.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 enrollment data (2023-24 school year)
#' enr_2024 <- fetch_enr(2024)
#'
#' # Get wide format
#' enr_wide <- fetch_enr(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' enr_fresh <- fetch_enr(2024, use_cache = FALSE)
#'
#' # Use 180-day count instead of 45-day
#' enr_180 <- fetch_enr(2024, count_day = "180")
#'
#' # Filter to specific district
#' greenville <- enr_2024 %>%
#'   dplyr::filter(grepl("Greenville", district_name))
#' }
fetch_enr <- function(end_year, tidy = TRUE, use_cache = TRUE, count_day = "45") {

  # Validate year
  if (end_year < 2013 || end_year > 2026) {
    stop(paste0(
      "end_year must be between 2013 and 2026.\n",
      "SC Active Student Headcount data is available from 2012-13 school year.\n",
      "Use get_available_years() to see all available years."
    ))
  }

  # Validate count_day
  if (!count_day %in% c("45", "135", "180")) {
    stop("count_day must be '45', '135', or '180'")
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && cache_exists(end_year, cache_type)) {
    message(paste("Using cached data for", end_year))
    return(read_cache(end_year, cache_type))
  }

  # Get raw data from SCDE
  raw <- get_raw_enr(end_year, count_day)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) %>%
      id_enr_aggs()
  }

  # Cache the result
  if (use_cache) {
    write_cache(processed, end_year, cache_type)
  }

  processed
}


#' Fetch enrollment data for multiple years
#'
#' Downloads and combines enrollment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @param count_day Which count day to use: "45" (default), "135", or "180".
#' @return Combined data frame with enrollment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data
#' enr_multi <- fetch_enr_multi(2022:2024)
#'
#' # Track enrollment trends
#' enr_multi %>%
#'   dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
#'   dplyr::select(end_year, n_students)
#' }
fetch_enr_multi <- function(end_years, tidy = TRUE, use_cache = TRUE, count_day = "45") {

  # Validate years
  invalid_years <- end_years[end_years < 2013 | end_years > 2026]
  if (length(invalid_years) > 0) {
    stop(paste("Invalid years:", paste(invalid_years, collapse = ", "),
               "\nend_year must be between 2013 and 2026"))
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_enr(yr, tidy = tidy, use_cache = use_cache, count_day = count_day)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}


#' Fetch enrollment data from SC Report Cards
#'
#' Downloads enrollment data from the SC Report Cards website.
#' This source provides basic enrollment counts and school information
#' but less demographic detail than Active Student Headcounts.
#'
#' @param end_year School year end (e.g., 2024 for 2023-24). Valid: 2018-2025.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Data frame with school enrollment and basic information
#' @export
#' @examples
#' \dontrun{
#' # Get Report Cards data
#' rc_2024 <- fetch_report_cards(2024)
#'
#' # View school types
#' table(rc_2024$school_type)
#' }
fetch_report_cards <- function(end_year, use_cache = TRUE) {

  # Validate year
  if (end_year < 2018 || end_year > 2025) {
    stop("SC Report Cards data available from 2018-2025")
  }

  # Check cache
  cache_type <- "reportcards"
  if (use_cache && cache_exists(end_year, cache_type)) {
    message(paste("Using cached Report Cards data for", end_year))
    return(read_cache(end_year, cache_type))
  }

  message(paste("Downloading SC Report Cards data for", end_year, "..."))

  # Download data
  df <- download_report_cards_data(end_year)

  # Standardize columns
  result <- data.frame(
    end_year = end_year,
    school_id = df$schoolid,
    district_name = clean_text(df$districtnm),
    school_name = clean_text(df$schoolnm),
    school_type = df[[grep("type", names(df), value = TRUE)[1]]],
    enrollment = safe_numeric(df$enrollment),
    teacher_count = safe_numeric(df$teachercount),
    grade_span = df$gradespan,
    address = paste(df$street, df$city, df$state, df$zip, sep = ", "),
    stringsAsFactors = FALSE
  )

  # Cache
  if (use_cache) {
    write_cache(result, end_year, cache_type)
  }

  result
}
