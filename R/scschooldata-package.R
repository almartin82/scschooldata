#' @keywords internal
"_PACKAGE"

#' scschooldata: South Carolina School Enrollment Data
#'
#' The scschooldata package provides functions for downloading, processing,
#' and analyzing school enrollment data from the South Carolina Department
#' of Education (SCDE).
#'
#' @section Data Sources:
#' The package uses two primary data sources:
#' \itemize{
#'   \item \strong{Active Student Headcounts} (ed.sc.gov): The primary source,
#'     providing school and district-level enrollment by grade and demographics.
#'     Available from 2012-13 school year onwards.
#'   \item \strong{SC Report Cards} (screportcards.com): Secondary source with
#'     comprehensive school information. Available from 2017-18 school year onwards.
#' }
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{fetch_enr}}}{Download and process enrollment data for a single year}
#'   \item{\code{\link{fetch_enr_multi}}}{Download enrollment data for multiple years}
#'   \item{\code{\link{tidy_enr}}}{Convert wide enrollment data to long format}
#'   \item{\code{\link{get_available_years}}}{List available data years}
#' }
#'
#' @section Caching:
#' Downloaded data is cached locally to avoid repeated downloads. Use
#' \code{\link{cache_status}} to view cached files and \code{\link{clear_cache}}
#' to remove them.
#'
#' @docType package
#' @name scschooldata-package
NULL
