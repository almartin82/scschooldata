# Tests for enrollment functions
# Note: Most tests are marked as skip_on_cran since they require network access

test_that("safe_numeric handles various inputs", {
  # Normal numbers
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)

  # Suppressed values
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("N<10")))

  # Whitespace handling
  expect_equal(safe_numeric("  100  "), 100)
})

test_that("get_available_years returns valid range", {
  years <- get_available_years()
  expect_true(is.integer(years) || is.numeric(years))
  expect_true(min(years) >= 2018)
  expect_true(max(years) <= 2026)
  expect_true(length(years) > 0)
})

test_that("fetch_enr validates year parameter", {
  expect_error(fetch_enr(2010), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
})

test_that("fetch_enr validates count_day parameter", {
  expect_error(fetch_enr(2024, count_day = "50"), "count_day must be")
})

test_that("get_cache_dir returns valid path", {
  cache_dir <- get_cache_dir()
  expect_true(is.character(cache_dir))
  expect_true(grepl("scschooldata", cache_dir))
})

test_that("cache functions work correctly", {
  # Test cache path generation
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024.rds", path))

  # Test cache_exists returns FALSE for non-existent cache
  expect_false(cache_exists(9999, "tidy"))
})

test_that("clean_text removes line breaks", {
  expect_equal(clean_text("Hello\r\nWorld"), "Hello World")
  expect_equal(clean_text("  Multiple   Spaces  "), "Multiple Spaces")
  expect_equal(clean_text("Test\rText\nHere"), "Test Text Here")
})

test_that("generate_district_id produces consistent output", {
  id1 <- generate_district_id("Test District")
  id2 <- generate_district_id("Test District")
  expect_equal(id1, id2)
  expect_equal(nchar(id1), 3)
})

test_that("get_grade_column_names returns expected columns", {
  school_cols <- get_grade_column_names("school")
  district_cols <- get_grade_column_names("district")

  expect_true("school_id" %in% school_cols)
  expect_false("school_id" %in% district_cols)
  expect_true("grade_01" %in% school_cols)
  expect_true("grade_01" %in% district_cols)
})

test_that("get_demo_column_names returns expected columns", {
  school_cols <- get_demo_column_names("school")
  district_cols <- get_demo_column_names("district")

  expect_true("school_id" %in% school_cols)
  expect_false("school_id" %in% district_cols)
  expect_true("black" %in% school_cols)
  expect_true("hispanic" %in% district_cols)
})

test_that("get_sc_district_codes returns named vector", {
  codes <- get_sc_district_codes()
  expect_true(is.character(codes))
  expect_true(length(names(codes)) > 0)
  expect_true("Greenville County Schools" %in% names(codes))
})

# Integration tests (require network access)
test_that("fetch_enr downloads and processes data", {
  skip_on_cran()
  skip_if_offline()

  # Use a recent year
  result <- fetch_enr(2024, tidy = FALSE, use_cache = FALSE)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("district_id" %in% names(result))
  expect_true("campus_id" %in% names(result))
  expect_true("row_total" %in% names(result))
  expect_true("type" %in% names(result))

  # Check we have all levels
  expect_true("State" %in% result$type)
  expect_true("District" %in% result$type)
  expect_true("Campus" %in% result$type)

  # Check we have enrollment data
  state_row <- result[result$type == "State", ]
  expect_true(state_row$row_total > 500000)  # SC has ~750k students
})

test_that("tidy_enr produces correct long format", {
  skip_on_cran()
  skip_if_offline()

  # Get wide data
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Tidy it
  tidy_result <- tidy_enr(wide)

  # Check structure
  expect_true("grade_level" %in% names(tidy_result))
  expect_true("subgroup" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))

  # Check subgroups include expected values
  subgroups <- unique(tidy_result$subgroup)
  expect_true("total_enrollment" %in% subgroups)
})

test_that("id_enr_aggs adds correct flags", {
  skip_on_cran()
  skip_if_offline()

  # Get tidy data with aggregation flags
  result <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Check flags exist
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))
  expect_true("is_charter" %in% names(result))

  # Check flags are boolean
  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))
  expect_true(is.logical(result$is_campus))
  expect_true(is.logical(result$is_charter))

  # Check mutual exclusivity (each row is only one type)
  type_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(type_sums == 1))
})

test_that("fetch_enr_multi returns combined data", {
  skip_on_cran()
  skip_if_offline()

  # Fetch 2 years
  result <- fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE)

  # Should have both years
  expect_true(2023 %in% result$end_year)
  expect_true(2024 %in% result$end_year)

  # Each year should have state total
  state_totals <- result %>%
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(state_totals), 2)
})

test_that("fetch_report_cards downloads data", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_report_cards(2024, use_cache = FALSE)

  expect_true(is.data.frame(result))
  expect_true("school_id" %in% names(result))
  expect_true("enrollment" %in% names(result))
  expect_true(nrow(result) > 1000)  # SC has ~1400 schools
})

test_that("enr_grade_aggs creates grade aggregates", {
  skip_on_cran()
  skip_if_offline()

  tidy_data <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy_data)

  # Check grade level aggregates exist
  expect_true("K8" %in% aggs$grade_level)
  expect_true("HS" %in% aggs$grade_level)
  expect_true("K12" %in% aggs$grade_level)

  # K-12 should be sum of K-8 and HS
  state_aggs <- aggs %>%
    dplyr::filter(is_state)
  k8 <- state_aggs$n_students[state_aggs$grade_level == "K8"]
  hs <- state_aggs$n_students[state_aggs$grade_level == "HS"]
  k12 <- state_aggs$n_students[state_aggs$grade_level == "K12"]
  expect_equal(k8 + hs, k12)
})
