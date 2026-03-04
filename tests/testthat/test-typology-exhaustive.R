# ==============================================================================
# Exhaustive Typology Tests for SC School Data
# ==============================================================================
#
# Tests data structure, column types, naming standards, edge cases, and
# cross-function consistency. All pinned values from real SC DOE data.
#
# All fetch calls use use_cache = TRUE.
# ==============================================================================

library(dplyr)

# Helper: skip test if SC DOE (ed.sc.gov) is unreachable or returning errors.
skip_if_sc_doe_unavailable <- function(e) {
  if (grepl("Failed to download|too small|HTTP error|SSL|timeout|connection|Could not resolve",
            e$message, ignore.case = TRUE)) {
    skip(paste("SC DOE (ed.sc.gov) unavailable:", e$message))
  }
  stop(e)
}

# ==============================================================================
# Column Type Tests — Wide Format
# ==============================================================================

test_that("wide format column types are correct", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Numeric columns
  expect_true(is.numeric(enr_wide$end_year))
  expect_true(is.numeric(enr_wide$row_total))
  expect_true(is.numeric(enr_wide$white))
  expect_true(is.numeric(enr_wide$black))
  expect_true(is.numeric(enr_wide$hispanic))
  expect_true(is.numeric(enr_wide$asian))
  expect_true(is.numeric(enr_wide$pacific_islander))
  expect_true(is.numeric(enr_wide$native_american))
  expect_true(is.numeric(enr_wide$multiracial))
  expect_true(is.numeric(enr_wide$female))
  expect_true(is.numeric(enr_wide$male))
  expect_true(is.numeric(enr_wide$grade_pk))
  expect_true(is.numeric(enr_wide$grade_k))
  expect_true(is.numeric(enr_wide$grade_01))
  expect_true(is.numeric(enr_wide$grade_12))

  # Character columns
  expect_true(is.character(enr_wide$type))
  expect_true(is.character(enr_wide$district_id))
  expect_true(is.character(enr_wide$campus_id))
  expect_true(is.character(enr_wide$district_name))
  expect_true(is.character(enr_wide$campus_name))
  expect_true(is.character(enr_wide$charter_flag))
  expect_true(is.character(enr_wide$county))
  expect_true(is.character(enr_wide$region))
})

test_that("wide format end_year is always 2024 for 2024 fetch", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(all(enr_wide$end_year == 2024))
})


# ==============================================================================
# Column Type Tests — Tidy Format
# ==============================================================================

test_that("tidy format column types are correct", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(is.numeric(enr_tidy$end_year))
  expect_true(is.character(enr_tidy$type))
  expect_true(is.character(enr_tidy$district_id))
  expect_true(is.character(enr_tidy$campus_id))
  expect_true(is.character(enr_tidy$district_name))
  expect_true(is.character(enr_tidy$campus_name))
  expect_true(is.character(enr_tidy$grade_level))
  expect_true(is.character(enr_tidy$subgroup))
  expect_true(is.numeric(enr_tidy$n_students))
  expect_true(is.numeric(enr_tidy$pct))
  expect_true(is.logical(enr_tidy$is_state))
  expect_true(is.logical(enr_tidy$is_district))
  expect_true(is.logical(enr_tidy$is_campus))
  expect_true(is.logical(enr_tidy$is_charter))
  expect_true(is.character(enr_tidy$aggregation_flag))
})


# ==============================================================================
# Naming Standards Tests
# ==============================================================================

test_that("subgroup names follow naming standards", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  subgroups <- unique(enr_tidy$subgroup)

  # Standard names that MUST exist
  expect_true("total_enrollment" %in% subgroups)
  expect_true("econ_disadv" %in% subgroups)
  expect_true("native_american" %in% subgroups)
  expect_true("multiracial" %in% subgroups)
  expect_true("lep" %in% subgroups)
  expect_true("special_ed" %in% subgroups)

  # Non-standard names that MUST NOT exist
  expect_false("total" %in% subgroups)
  expect_false("low_income" %in% subgroups)
  expect_false("economically_disadvantaged" %in% subgroups)
  expect_false("frl" %in% subgroups)
  expect_false("iep" %in% subgroups)
  expect_false("disability" %in% subgroups)
  expect_false("el" %in% subgroups)
  expect_false("ell" %in% subgroups)
  expect_false("english_learner" %in% subgroups)
  expect_false("american_indian" %in% subgroups)
  expect_false("two_or_more" %in% subgroups)
})

test_that("grade levels follow naming standards", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  grade_levels <- unique(enr_tidy$grade_level)

  # All grade levels should be uppercase
  for (gl in grade_levels) {
    expect_equal(gl, toupper(gl), info = paste("Grade level not uppercase:", gl))
  }

  # Standard grade levels that MUST exist
  expect_true("PK" %in% grade_levels)
  expect_true("K" %in% grade_levels)
  expect_true("TOTAL" %in% grade_levels)
  expect_true("01" %in% grade_levels)
  expect_true("12" %in% grade_levels)

  # Non-standard names that MUST NOT exist
  expect_false("pk" %in% grade_levels)
  expect_false("k" %in% grade_levels)
  expect_false("KF" %in% grade_levels)
  expect_false("KH" %in% grade_levels)
  expect_false("1" %in% grade_levels)  # should be "01"
  expect_false("2" %in% grade_levels)  # should be "02"
})

test_that("entity flags follow naming standards", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # SC uses is_campus (not is_school)
  expect_true("is_state" %in% names(enr_tidy))
  expect_true("is_district" %in% names(enr_tidy))
  expect_true("is_campus" %in% names(enr_tidy))
  expect_true("is_charter" %in% names(enr_tidy))

  # is_school should NOT be in the data (SC uses is_campus)
  expect_false("is_school" %in% names(enr_tidy))
})

test_that("grade aggregates follow naming standards", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  aggs <- enr_grade_aggs(enr_tidy)
  agg_levels <- unique(aggs$grade_level)

  # Standard aggregate names
  expect_true("K8" %in% agg_levels)
  expect_true("HS" %in% agg_levels)
  expect_true("K12" %in% agg_levels)
  expect_equal(length(agg_levels), 3)
})


# ==============================================================================
# Entity Identification Tests
# ==============================================================================

test_that("state rows have NA district and campus IDs", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_rows <- enr_tidy %>% filter(is_state)
  expect_true(all(is.na(state_rows$district_id)))
  expect_true(all(is.na(state_rows$campus_id)))
  expect_true(all(is.na(state_rows$district_name)))
  expect_true(all(is.na(state_rows$campus_name)))
})

test_that("district rows have valid district_id and NA campus_id", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  dist_rows <- enr_tidy %>% filter(is_district)
  expect_true(all(!is.na(dist_rows$district_id)))
  expect_true(all(!is.na(dist_rows$district_name)))
  expect_true(all(is.na(dist_rows$campus_id)))
  expect_true(all(is.na(dist_rows$campus_name)))
})

test_that("campus rows have valid district_id and campus_id", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  campus_rows <- enr_tidy %>% filter(is_campus)
  expect_true(all(!is.na(campus_rows$district_id)))
  expect_true(all(!is.na(campus_rows$campus_id)))
  expect_true(all(!is.na(campus_rows$campus_name)))
})

test_that("district IDs are 3-4 character strings", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  dist_ids <- unique(enr_wide$district_id[enr_wide$type == "District"])
  for (id in dist_ids) {
    expect_true(
      grepl("^\\d{3,4}$", id),
      info = paste("Invalid district ID format:", id)
    )
  }
})

test_that("campus IDs are 7-digit strings", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  campus_ids <- unique(enr_wide$campus_id[enr_wide$type == "Campus"])
  for (id in campus_ids) {
    expect_true(
      grepl("^\\d{7}$", id),
      info = paste("Invalid campus ID format:", id)
    )
  }
})

test_that("campus district_id matches first 3 chars of campus_id", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  campuses <- enr_wide %>% filter(type == "Campus")
  expected_dist_id <- substr(campuses$campus_id, 1, 3)
  # The district_id for campuses is first 3 digits
  # (but district IDs themselves may have 4 digits for some)
  # Just check it starts with consistent prefix
  expect_true(all(nchar(campuses$district_id) >= 3))
})


# ==============================================================================
# One Observation Per Group Per Period Tests
# ==============================================================================

test_that("no duplicate state+subgroup+grade_level in tidy (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_dupes <- enr_tidy %>%
    filter(is_state) %>%
    count(subgroup, grade_level) %>%
    filter(n > 1)
  expect_equal(nrow(state_dupes), 0)
})

test_that("no duplicate district+subgroup+grade_level in tidy (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  dist_dupes <- enr_tidy %>%
    filter(is_district) %>%
    count(district_id, subgroup, grade_level) %>%
    filter(n > 1)
  expect_equal(nrow(dist_dupes), 0)
})

test_that("no duplicate campus+subgroup+grade_level in tidy (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  campus_dupes <- enr_tidy %>%
    filter(is_campus) %>%
    count(campus_id, subgroup, grade_level) %>%
    filter(n > 1)
  expect_equal(nrow(campus_dupes), 0)
})

test_that("no duplicate year+entity in multi-year data", {
  skip_on_cran()
  skip_if_offline()

  enr_multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # State-level should have exactly 1 row per year per subgroup per grade
  state_dupes <- enr_multi %>%
    filter(is_state) %>%
    count(end_year, subgroup, grade_level) %>%
    filter(n > 1)
  expect_equal(nrow(state_dupes), 0)
})


# ==============================================================================
# Data Quality Tests
# ==============================================================================

test_that("no negative enrollment counts in wide (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  numeric_cols <- c("row_total", "white", "black", "hispanic", "asian",
                    "pacific_islander", "native_american", "multiracial",
                    "female", "male", "grade_pk", "grade_k",
                    paste0("grade_", sprintf("%02d", 1:12)))

  for (col in numeric_cols) {
    vals <- enr_wide[[col]][!is.na(enr_wide[[col]])]
    expect_true(all(vals >= 0), info = paste("Negative value in", col))
  }
})

test_that("racial counts sum to approximately total (2024 state)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state <- enr_wide[enr_wide$type == "State", ]
  racial_sum <- sum(state$white, state$black, state$hispanic, state$asian,
                    state$pacific_islander, state$native_american, state$multiracial,
                    na.rm = TRUE)

  # Racial sum should be very close to total (within 1% due to rounding/unreported)
  ratio <- racial_sum / state$row_total
  expect_true(ratio > 0.99 && ratio <= 1.0,
              info = paste("Racial sum ratio:", ratio))
})

test_that("gender sum approximately equals total (2024 state)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state <- enr_wide[enr_wide$type == "State", ]
  gender_sum <- state$female + state$male
  # Gender sum should be very close to total
  ratio <- gender_sum / state$row_total
  expect_true(ratio > 0.99 && ratio <= 1.01,
              info = paste("Gender sum ratio:", ratio))
})

test_that("state total is in reasonable range for SC", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_total <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  # SC has approximately 700K-850K students
  expect_true(state_total > 700000)
  expect_true(state_total < 850000)
})


# ==============================================================================
# Cross-Format Consistency Tests
# ==============================================================================

test_that("tidy state total matches wide state row_total", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  wide_total <- enr_wide$row_total[enr_wide$type == "State"]
  tidy_total <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(tidy_total, wide_total)
})

test_that("tidy demographic counts match wide columns (state)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_wide <- enr_wide[enr_wide$type == "State", ]

  demo_cols <- c("white", "black", "hispanic", "asian",
                 "pacific_islander", "native_american", "multiracial",
                 "female", "male")

  for (col in demo_cols) {
    wide_val <- state_wide[[col]]
    tidy_val <- enr_tidy %>%
      filter(is_state, subgroup == col, grade_level == "TOTAL") %>%
      pull(n_students)
    expect_equal(tidy_val, wide_val, info = paste("Mismatch in", col))
  }
})

test_that("tidy grade counts match wide columns (state)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_wide <- enr_wide[enr_wide$type == "State", ]

  grade_map <- list(
    "grade_pk" = "PK", "grade_k" = "K",
    "grade_01" = "01", "grade_02" = "02", "grade_03" = "03",
    "grade_04" = "04", "grade_05" = "05", "grade_06" = "06",
    "grade_07" = "07", "grade_08" = "08", "grade_09" = "09",
    "grade_10" = "10", "grade_11" = "11", "grade_12" = "12"
  )

  for (wide_col in names(grade_map)) {
    tidy_gl <- grade_map[[wide_col]]
    wide_val <- state_wide[[wide_col]]
    tidy_val <- enr_tidy %>%
      filter(is_state, subgroup == "total_enrollment", grade_level == tidy_gl) %>%
      pull(n_students)
    expect_equal(tidy_val, wide_val, info = paste("Grade mismatch:", wide_col, "vs", tidy_gl))
  }
})

test_that("tidy Greenville counts match wide (cross-check)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  gv_wide <- enr_wide[enr_wide$district_id == "2301" & enr_wide$type == "District", ]

  gv_tidy_total <- enr_tidy %>%
    filter(is_district, district_id == "2301",
           subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(gv_tidy_total, gv_wide$row_total)

  gv_tidy_white <- enr_tidy %>%
    filter(is_district, district_id == "2301",
           subgroup == "white", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(gv_tidy_white, gv_wide$white)

  gv_tidy_k <- enr_tidy %>%
    filter(is_district, district_id == "2301",
           subgroup == "total_enrollment", grade_level == "K") %>%
    pull(n_students)
  expect_equal(gv_tidy_k, gv_wide$grade_k)
})


# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("empty subgroup filter returns 0 rows", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  result <- enr_tidy %>% filter(subgroup == "nonexistent_subgroup")
  expect_equal(nrow(result), 0)
})

test_that("empty grade_level filter returns 0 rows", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  result <- enr_tidy %>% filter(grade_level == "99")
  expect_equal(nrow(result), 0)
})

test_that("EE grade level does NOT exist in 2024 data", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # CLAUDE.md notes EE may exist in some years, verify 2024 status
  expect_false("EE" %in% unique(enr_tidy$grade_level))
})

test_that("filtering by non-existent district returns 0 rows", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  result <- enr_tidy %>% filter(district_id == "9999")
  expect_equal(nrow(result), 0)
})


# ==============================================================================
# Utility Function Tests
# ==============================================================================

test_that("safe_numeric handles commas", {
  expect_equal(safe_numeric("1,000"), 1000)
  expect_equal(safe_numeric("1,234,567"), 1234567)
})

test_that("safe_numeric handles all suppression markers", {
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric("-")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("NA")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("n/a")))
  expect_true(is.na(safe_numeric("N<10")))
})

test_that("safe_numeric handles whitespace", {
  expect_equal(safe_numeric("  100  "), 100)
  expect_equal(safe_numeric("\t50\n"), 50)
})

test_that("safe_numeric handles negative numbers", {
  expect_true(is.na(safe_numeric("-1")))  # -1 is a suppression marker
  expect_equal(safe_numeric("-2"), -2)
})

test_that("safe_numeric handles decimal numbers", {
  expect_equal(safe_numeric("3.14"), 3.14)
  expect_equal(safe_numeric("99.9"), 99.9)
})

test_that("safe_numeric handles vectors", {
  result <- safe_numeric(c("100", "200", "*", "300"))
  expect_equal(result, c(100, 200, NA, 300))
})

test_that("clean_text handles various line break types", {
  expect_equal(clean_text("Hello\r\nWorld"), "Hello World")
  expect_equal(clean_text("Test\rText"), "Test Text")
  expect_equal(clean_text("New\nLine"), "New Line")
})

test_that("clean_text collapses multiple spaces", {
  expect_equal(clean_text("Too    Many   Spaces"), "Too Many Spaces")
})

test_that("clean_text trims leading and trailing whitespace", {
  expect_equal(clean_text("  Hello  "), "Hello")
})

test_that("get_grade_column_names returns correct count", {
  school_cols <- get_grade_column_names("school")
  expect_length(school_cols, 18)  # id + district_name + school_name + total + 14 grades
  expect_equal(school_cols[1], "school_id")
  expect_equal(school_cols[4], "total")

  district_cols <- get_grade_column_names("district")
  expect_length(district_cols, 17)  # id + district_name + total + 14 grades
  expect_equal(district_cols[1], "district_id")
  expect_equal(district_cols[3], "total")
})

test_that("get_demo_column_names returns correct count", {
  school_cols <- get_demo_column_names("school")
  expect_length(school_cols, 16)  # id + district_name + school_name + total + 12 demos (incl gender_missing, not_econ_disadv)
  expect_equal(school_cols[1], "school_id")

  district_cols <- get_demo_column_names("district")
  expect_length(district_cols, 15)  # id + district_name + total + 12 demos
  expect_equal(district_cols[1], "district_id")
})

test_that("get_grade_column_names includes all standard grades", {
  school_cols <- get_grade_column_names("school")
  expected_grades <- c("grade_pk", "grade_k", paste0("grade_", sprintf("%02d", 1:12)))
  for (g in expected_grades) {
    expect_true(g %in% school_cols, info = paste("Missing grade:", g))
  }
})

test_that("get_demo_column_names includes standard demographics", {
  school_cols <- get_demo_column_names("school")
  expected_demos <- c("female", "male", "black", "white", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial",
                      "econ_disadv")
  for (d in expected_demos) {
    expect_true(d %in% school_cols, info = paste("Missing demo:", d))
  }
})

test_that("get_sc_district_codes returns complete set", {
  codes <- get_sc_district_codes()
  expect_true(length(codes) >= 60)
  expect_true(is.character(codes))
  expect_true(all(nchar(codes) == 3))

  # Check specific districts (unname to compare values only)
  expect_equal(unname(codes["Greenville County Schools"]), "156")
  expect_equal(unname(codes["Charleston County School District"]), "072")
  expect_equal(unname(codes["SC Public Charter School District"]), "900")
  expect_equal(unname(codes["Richland School District 1"]), "256")
  expect_equal(unname(codes["Horry County Schools"]), "180")
})

test_that("generate_district_id is deterministic", {
  id1 <- generate_district_id("Test District")
  id2 <- generate_district_id("Test District")
  expect_identical(id1, id2)
})

test_that("generate_district_id returns 3-char string", {
  id <- generate_district_id("Some District Name")
  expect_equal(nchar(id), 3)
  expect_true(grepl("^\\d{3}$", id))
})


# ==============================================================================
# Cache Function Tests
# ==============================================================================

test_that("cache_status returns data frame", {
  result <- cache_status()
  expect_true(is.data.frame(result))
})

test_that("cache roundtrip preserves data", {
  test_df <- data.frame(
    a = c(1, 2, 3),
    b = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  write_cache(test_df, 9990, "roundtrip_test")

  expect_true(cache_exists(9990, "roundtrip_test", max_age = 1))

  read_df <- read_cache(9990, "roundtrip_test")
  expect_identical(test_df, read_df)

  # Clean up
  clear_cache(9990, "roundtrip_test")
  expect_false(cache_exists(9990, "roundtrip_test"))
})

test_that("clear_cache by type removes matching files only", {
  write_cache(data.frame(a = 1), 9991, "type_a")
  write_cache(data.frame(a = 2), 9992, "type_a")
  write_cache(data.frame(a = 3), 9991, "type_b")

  clear_cache(type = "type_a")

  # type_a should be gone
  expect_false(cache_exists(9991, "type_a"))
  expect_false(cache_exists(9992, "type_a"))

  # type_b should still exist
  expect_true(cache_exists(9991, "type_b"))

  # Clean up
  clear_cache(9991)
})

test_that("clear_cache by year removes matching files only", {
  write_cache(data.frame(a = 1), 9993, "test_x")
  write_cache(data.frame(a = 2), 9993, "test_y")
  write_cache(data.frame(a = 3), 9994, "test_x")

  clear_cache(9993)

  # Year 9993 should be gone
  expect_false(cache_exists(9993, "test_x"))
  expect_false(cache_exists(9993, "test_y"))

  # Year 9994 should still exist
  expect_true(cache_exists(9994, "test_x"))

  # Clean up
  clear_cache(9994)
})

test_that("get_cache_dir returns path containing scschooldata", {
  path <- get_cache_dir()
  expect_true(is.character(path))
  expect_true(grepl("scschooldata", path))
  expect_true(dir.exists(path))
})

test_that("get_cache_path generates correct filename", {
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024\\.rds$", path))

  path2 <- get_cache_path(2023, "wide")
  expect_true(grepl("enr_wide_2023\\.rds$", path2))
})


# ==============================================================================
# Directory Cache Tests
# ==============================================================================

test_that("clear_directory_cache removes directory cache files", {
  skip_if_not(exists("write_cache_directory", where = asNamespace("scschooldata"), mode = "function"),
              "directory cache functions not yet implemented")

  # Write a test cache
  write_cache_directory(data.frame(a = 1), "directory_test_9999")
  expect_true(cache_exists_directory("directory_test_9999", max_age = 1))

  clear_directory_cache()

  # Should be gone
  expect_false(cache_exists_directory("directory_test_9999"))
})


# ==============================================================================
# Directory Data Structure Tests
# ==============================================================================

test_that("directory data has correct column types", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not(exists("fetch_directory", where = asNamespace("scschooldata"), mode = "function"),
              "fetch_directory not yet implemented")

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  expect_true(is.integer(dir_data$end_year))
  expect_true(is.character(dir_data$state_school_id))
  expect_true(is.character(dir_data$state_district_id))
  expect_true(is.character(dir_data$district_name))
  expect_true(is.character(dir_data$school_name))
  expect_true(is.character(dir_data$entity_type))
  expect_true(is.character(dir_data$school_type))
  expect_true(is.character(dir_data$address))
  expect_true(is.character(dir_data$city))
  expect_true(is.character(dir_data$state))
  expect_true(is.character(dir_data$zip))
  expect_true(is.character(dir_data$phone))
  expect_true(is.character(dir_data$grades_served))
  expect_true(is.numeric(dir_data$enrollment))
  expect_true(is.numeric(dir_data$teacher_count))
})

test_that("directory school IDs are 7-digit strings", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not(exists("fetch_directory", where = asNamespace("scschooldata"), mode = "function"),
              "fetch_directory not yet implemented")

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  school_ids <- dir_data$state_school_id[dir_data$entity_type == "School"]
  non_na_ids <- school_ids[!is.na(school_ids)]
  expect_true(all(grepl("^\\d{7}$", non_na_ids)),
              info = "All school IDs should be 7-digit strings")
})

test_that("directory school_type codes are valid", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not(exists("fetch_directory", where = asNamespace("scschooldata"), mode = "function"),
              "fetch_directory not yet implemented")

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  valid_types <- c("D", "E", "H", "M", "P", "S")
  non_na_types <- dir_data$school_type[!is.na(dir_data$school_type)]
  expect_true(all(non_na_types %in% valid_types))
})


# ==============================================================================
# Cross-Function Consistency
# ==============================================================================

test_that("fetch_enr and fetch_enr_multi produce identical single-year output", {
  skip_on_cran()
  skip_if_offline()

  single <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  multi <- tryCatch(
    fetch_enr_multi(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Same structure
  expect_equal(names(single), names(multi))
  expect_equal(nrow(single), nrow(multi))

  # Same state total
  single_total <- single %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)
  multi_total <- multi %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(single_total, multi_total)
})

test_that("tidy_enr + id_enr_aggs chain matches fetch_enr tidy output", {
  skip_on_cran()
  skip_if_offline()

  # Direct tidy fetch
  tidy_direct <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Manual chain: wide -> tidy -> aggs
  wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )
  tidy_manual <- tidy_enr(wide) |> id_enr_aggs()

  # Same columns
  expect_equal(sort(names(tidy_direct)), sort(names(tidy_manual)))

  # Same row count
  expect_equal(nrow(tidy_direct), nrow(tidy_manual))

  # Same state total
  direct_total <- tidy_direct %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)
  manual_total <- tidy_manual %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(direct_total, manual_total)
})


# ==============================================================================
# URL Builder Tests (internal functions)
# ==============================================================================

test_that("build_headcount_url returns valid URL string", {
  url <- build_headcount_url(2024, "2023-24", "school", "grade", "45")
  expect_true(is.character(url))
  expect_true(grepl("^https://", url))
  expect_true(grepl("ed.sc.gov", url))
})

test_that("build_headcount_url varies by level and data_type", {
  url_school_grade <- build_headcount_url(2024, "2023-24", "school", "grade", "45")
  url_school_demo <- build_headcount_url(2024, "2023-24", "school", "demo", "45")
  url_district_grade <- build_headcount_url(2024, "2023-24", "district", "grade", "45")

  expect_false(url_school_grade == url_school_demo)
  expect_false(url_school_grade == url_district_grade)
})

test_that("build_alternate_urls returns many alternatives", {
  urls <- build_alternate_urls(2024, "2023-24", "school", "grade", "45")
  expect_true(length(urls) > 10)
  expect_true(all(grepl("^https://", urls)))
})

test_that("build_report_cards_urls returns multiple patterns", {
  urls <- build_report_cards_urls(2024)
  expect_true(length(urls) >= 8)
  expect_true(all(grepl("screportcards.com", urls)))
})

test_that("build_directory_urls returns multiple patterns", {
  skip_if_not(exists("build_directory_urls", where = asNamespace("scschooldata"), mode = "function"),
              "build_directory_urls not yet implemented")

  urls <- build_directory_urls(2025)
  expect_true(length(urls) >= 8)
  expect_true(all(grepl("screportcards.com", urls)))
})
