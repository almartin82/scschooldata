# ==============================================================================
# Utility Functions
# ==============================================================================

# Declare global variables to avoid R CMD check NOTEs
utils::globalVariables(c(
  "subgroup", "grade_level", "n_students", "row_total",
  "type", "charter_flag", "is_state", "is_district", "is_campus", "is_charter"
))

#' @importFrom stats setNames
NULL

#' @importFrom rlang .data
NULL


#' Convert to numeric, handling suppression markers
#'
#' SCDE uses various markers for suppressed data (*, <5, N/A, etc.)
#' and may use commas in large numbers.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  x[x %in% c("*", ".", "-", "-1", "<5", "N/A", "NA", "", "n/a", "N<10")] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Get list of available years for SC enrollment data
#'
#' Returns the range of years available from different SC data sources.
#'
#' @param source Which data source: "headcounts" (default) for Active Student
#'   Headcounts (2013-2024), or "reportcards" for SC Report Cards (2018-2024).
#' @return Integer vector of available years (end_year format, e.g., 2024 for 2023-24)
#' @export
#' @examples
#' get_available_years()
#' get_available_years("headcounts")
#' get_available_years("reportcards")
get_available_years <- function(source = "headcounts") {
  source <- match.arg(source, c("headcounts", "reportcards"))

  if (source == "headcounts") {
    # Active Student Headcounts available from 2012-13 through 2023-24
    # Uses end_year convention (2013 = 2012-13 school year)
    2013:2024
  } else {
    # SC Report Cards data available from 2018-2024
    2018:2024
  }
}
