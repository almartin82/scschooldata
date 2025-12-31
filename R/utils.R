# ==============================================================================
# Utility Functions
# ==============================================================================

#' Pipe operator
#'
#' See \code{dplyr::\link[dplyr:reexports]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
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
#' Returns the range of years available from SC Report Cards.
#'
#' @return Integer vector of available years
#' @export
#' @examples
#' get_available_years()
get_available_years <- function() {
  # SC Report Cards data available from 2018-2025
  # Active Student Headcounts goes back to 2016 but with different format
  2018:2025
}
