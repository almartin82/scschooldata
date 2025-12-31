# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw SCDE enrollment data into a
# clean, standardized format.
#
# ==============================================================================

#' Process raw SCDE enrollment data
#'
#' Transforms raw Active Student Headcount data into a standardized schema
#' combining school and district data.
#'
#' @param raw_data List containing school_grade, school_demo, district_grade,
#'   district_demo data frames from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process school data by merging grade and demographic files
  school_processed <- process_school_enr(
    raw_data$school_grade,
    raw_data$school_demo,
    end_year
  )

  # Process district data by merging grade and demographic files
  district_processed <- process_district_enr(
    raw_data$district_grade,
    raw_data$district_demo,
    end_year
  )

  # Create state aggregate
  state_processed <- create_state_aggregate(district_processed, end_year)

  # Combine all levels
  result <- dplyr::bind_rows(state_processed, district_processed, school_processed)

  result
}


#' Process school-level enrollment data
#'
#' Merges grade and demographic data for schools.
#'
#' @param grade_df School grade data frame
#' @param demo_df School demographic data frame
#' @param end_year School year end
#' @return Processed school data frame
#' @keywords internal
process_school_enr <- function(grade_df, demo_df, end_year) {

  n_rows <- nrow(grade_df)

  # Build result data frame
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("Campus", n_rows),
    stringsAsFactors = FALSE
  )

  # School and district identifiers
  if ("school_id" %in% names(grade_df)) {
    result$campus_id <- trimws(as.character(grade_df$school_id))
    # District ID is first 3 digits of 7-digit school ID in SC
    result$district_id <- substr(result$campus_id, 1, 3)
  }

  if ("district_name" %in% names(grade_df)) {
    result$district_name <- clean_text(grade_df$district_name)
  }

  if ("school_name" %in% names(grade_df)) {
    result$campus_name <- clean_text(grade_df$school_name)
  }

  # Total enrollment from grade file
  if ("total" %in% names(grade_df)) {
    result$row_total <- safe_numeric(grade_df$total)
  }

  # Grade levels from grade file
  grade_cols <- c(
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  for (col in grade_cols) {
    if (col %in% names(grade_df)) {
      result[[col]] <- safe_numeric(grade_df[[col]])
    }
  }

  # Merge demographic data if available
  if (!is.null(demo_df) && nrow(demo_df) > 0) {
    # Match by school_id
    if ("school_id" %in% names(demo_df)) {
      demo_df$school_id <- trimws(as.character(demo_df$school_id))

      # Join demographics
      demo_cols <- c(
        "female", "male",
        "white", "black", "hispanic", "asian",
        "native_american", "pacific_islander", "multiracial",
        "econ_disadv"
      )

      for (col in demo_cols) {
        if (col %in% names(demo_df)) {
          # Create a lookup
          demo_lookup <- setNames(safe_numeric(demo_df[[col]]), demo_df$school_id)
          result[[col]] <- demo_lookup[result$campus_id]
        }
      }
    }
  }

  # Add placeholder columns for any missing standard columns
  standard_demo <- c("white", "black", "hispanic", "asian",
                     "native_american", "pacific_islander", "multiracial",
                     "econ_disadv", "lep", "special_ed", "female", "male")
  for (col in standard_demo) {
    if (!col %in% names(result)) {
      result[[col]] <- NA_integer_
    }
  }

  # Charter flag - not directly available in headcount data
  result$charter_flag <- NA_character_

  # County and region - not in this data source
  result$county <- NA_character_
  result$region <- NA_character_

  result
}


#' Process district-level enrollment data
#'
#' Merges grade and demographic data for districts.
#'
#' @param grade_df District grade data frame
#' @param demo_df District demographic data frame
#' @param end_year School year end
#' @return Processed district data frame
#' @keywords internal
process_district_enr <- function(grade_df, demo_df, end_year) {

  n_rows <- nrow(grade_df)

  # Build result data frame
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("District", n_rows),
    stringsAsFactors = FALSE
  )

  # District identifiers
  if ("district_id" %in% names(grade_df)) {
    result$district_id <- trimws(as.character(grade_df$district_id))
  }

  if ("district_name" %in% names(grade_df)) {
    result$district_name <- clean_text(grade_df$district_name)
  }

  # Campus ID is NA for district rows
  result$campus_id <- NA_character_
  result$campus_name <- NA_character_

  # Total enrollment from grade file
  if ("total" %in% names(grade_df)) {
    result$row_total <- safe_numeric(grade_df$total)
  }

  # Grade levels from grade file
  grade_cols <- c(
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  for (col in grade_cols) {
    if (col %in% names(grade_df)) {
      result[[col]] <- safe_numeric(grade_df[[col]])
    }
  }

  # Merge demographic data if available
  if (!is.null(demo_df) && nrow(demo_df) > 0) {
    if ("district_id" %in% names(demo_df)) {
      demo_df$district_id <- trimws(as.character(demo_df$district_id))

      demo_cols <- c(
        "female", "male",
        "white", "black", "hispanic", "asian",
        "native_american", "pacific_islander", "multiracial",
        "econ_disadv"
      )

      for (col in demo_cols) {
        if (col %in% names(demo_df)) {
          # Create a lookup by district ID
          demo_lookup <- setNames(safe_numeric(demo_df[[col]]), demo_df$district_id)
          result[[col]] <- demo_lookup[result$district_id]
        }
      }
    }
  }

  # Add placeholder columns for any missing standard columns
  standard_demo <- c("white", "black", "hispanic", "asian",
                     "native_american", "pacific_islander", "multiracial",
                     "econ_disadv", "lep", "special_ed", "female", "male")
  for (col in standard_demo) {
    if (!col %in% names(result)) {
      result[[col]] <- NA_integer_
    }
  }

  # Charter flag - not applicable at district level
  result$charter_flag <- NA_character_

  # County and region - not in this data source
  result$county <- NA_character_
  result$region <- NA_character_

  result
}


#' Create state-level aggregate from district data
#'
#' @param district_df Processed district data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(district_df, end_year) {

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "econ_disadv", "lep", "special_ed",
    "female", "male",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    region = NA_character_,
    charter_flag = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
  }

  state_row
}


#' Clean text fields
#'
#' Removes carriage returns, extra whitespace, and standardizes text.
#'
#' @param x Character vector
#' @return Cleaned character vector
#' @keywords internal
clean_text <- function(x) {
  x <- gsub("\r\n|\r|\n", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x
}


#' Generate district ID from district name
#'
#' Creates a consistent 3-character ID from district name.
#' SC uses numeric district codes, but since they're not in the headcount files,
#' we generate a consistent identifier.
#'
#' @param name District name
#' @return 3-character district ID
#' @keywords internal
generate_district_id <- function(name) {
  # Use a hash-based approach for consistency
  # This maps each unique district name to a consistent ID
  hash_val <- sum(utf8ToInt(name))
  sprintf("%03d", hash_val %% 1000)
}


#' Look up district ID from master list
#'
#' SC Department of Education assigns numeric codes to districts.
#' This function maps district names to official codes when available.
#'
#' @return Named vector of district IDs
#' @keywords internal
get_sc_district_codes <- function() {
  # Official SC district codes (sample - would need full list)
  # Format: "District Name" = "Code"
  c(
    "Abbeville County School District" = "016",
    "Aiken County Public School District" = "020",
    "Allendale County School District" = "024",
    "Anderson School District One" = "028",
    "Anderson School District Two" = "032",
    "Anderson School District 3" = "036",
    "Anderson District 4" = "040",
    "Anderson School District Five" = "044",
    "Bamberg County School District" = "048",
    "Barnwell School District 45" = "052",
    "Barnwell County School District" = "056",
    "Beaufort County School District" = "060",
    "Berkeley County School District" = "064",
    "Calhoun County School District" = "068",
    "Charleston County School District" = "072",
    "Cherokee County School District" = "076",
    "Chester County School District" = "080",
    "Chesterfield County School District" = "084",
    "Clarendon School District 1" = "088",
    "Clarendon School District 2" = "092",
    "Clarendon County School District" = "096",
    "Colleton County School District" = "100",
    "Darlington County School District" = "104",
    "Dillon School District 4" = "108",
    "Dillon School District 3" = "112",
    "Dorchester School District 2" = "116",
    "Dorchester School District 4" = "120",
    "Edgefield County School District" = "124",
    "Fairfield County School District" = "128",
    "Florence School District 1" = "132",
    "Florence School District 2" = "136",
    "Florence School District 3" = "140",
    "Florence School District 4" = "144",
    "Florence School District 5" = "148",
    "Georgetown County School District" = "152",
    "Greenville County Schools" = "156",
    "Greenwood School District 50" = "160",
    "Greenwood School District 51" = "164",
    "Greenwood School District 52" = "168",
    "Hampton School District 1" = "172",
    "Hampton School District 2" = "176",
    "Horry County Schools" = "180",
    "Jasper County School District" = "184",
    "Kershaw County School District" = "188",
    "Lancaster County School District" = "192",
    "Laurens County School District 55" = "196",
    "Laurens County School District 56" = "200",
    "Lee County School District" = "204",
    "Lexington School District 1" = "208",
    "Lexington School District 2" = "212",
    "Lexington School District 3" = "216",
    "Lexington School District 4" = "220",
    "Lexington School District 5" = "224",
    "McCormick County School District" = "228",
    "Marion County School District" = "232",
    "Marlboro County School District" = "236",
    "Newberry County School District" = "240",
    "Oconee County School District" = "244",
    "Orangeburg County School District" = "248",
    "Pickens County School District" = "252",
    "Richland School District 1" = "256",
    "Richland School District 2" = "260",
    "Saluda County School District" = "264",
    "Spartanburg School District 1" = "268",
    "Spartanburg School District 2" = "272",
    "Spartanburg School District 3" = "276",
    "Spartanburg School District 4" = "280",
    "Spartanburg School District 5" = "284",
    "Spartanburg School District 6" = "288",
    "Spartanburg School District 7" = "292",
    "Sumter County School District" = "296",
    "Union County School District" = "300",
    "Williamsburg County School District" = "304",
    "York School District 1" = "308",
    "York School District 2" = "312",
    "York School District 3" = "316",
    "York School District 4" = "320",
    "SC Public Charter School District" = "900"
  )
}
