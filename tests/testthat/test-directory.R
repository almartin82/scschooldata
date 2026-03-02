# ==============================================================================
# Tests for fetch_directory()
# ==============================================================================

test_that("fetch_directory returns data", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- fetch_directory(use_cache = FALSE)

  expect_s3_class(dir_data, "tbl_df")
  expect_gt(nrow(dir_data), 100)
  expect_true("district_name" %in% names(dir_data))
  expect_true("school_name" %in% names(dir_data))
  expect_true("superintendent_name" %in% names(dir_data))
  expect_true("principal_name" %in% names(dir_data))
  expect_true("address" %in% names(dir_data))
  expect_true("city" %in% names(dir_data))
  expect_true("zip" %in% names(dir_data))
  expect_true("phone" %in% names(dir_data))
  expect_true("grades_served" %in% names(dir_data))
  expect_true("website" %in% names(dir_data))
  expect_true("entity_type" %in% names(dir_data))
  expect_true("state_school_id" %in% names(dir_data))
  expect_true("state_district_id" %in% names(dir_data))
})

test_that("fetch_directory raw format works", {
  skip_on_cran()
  skip_if_offline()

  dir_raw <- fetch_directory(tidy = FALSE, use_cache = FALSE)

  expect_s3_class(dir_raw, "tbl_df")
  expect_gt(nrow(dir_raw), 100)
})

test_that("fetch_directory has both school and district rows", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- fetch_directory(use_cache = TRUE)

  # Should have both entity types
  entity_types <- unique(dir_data$entity_type)
  expect_true("School" %in% entity_types)
  expect_true("District" %in% entity_types)

  # District rows should have superintendent names
  districts <- dir_data[dir_data$entity_type == "District", ]
  expect_gt(sum(!is.na(districts$superintendent_name)), 0)

  # School rows should have principal names
  schools <- dir_data[dir_data$entity_type == "School", ]
  expect_gt(sum(!is.na(schools$principal_name)), 0)
})

test_that("fetch_directory specific year works", {
  skip_on_cran()
  skip_if_offline()

  dir_2024 <- fetch_directory(2024, use_cache = FALSE)

  expect_s3_class(dir_2024, "tbl_df")
  expect_gt(nrow(dir_2024), 100)
  expect_equal(unique(dir_2024$end_year), 2024L)
})

test_that("fetch_directory validates year", {
  expect_error(fetch_directory(2017), "between 2018 and 2025")
  expect_error(fetch_directory(2026), "between 2018 and 2025")
})

test_that("directory cache works", {
  skip_on_cran()
  skip_if_offline()

  # First call caches
  dir1 <- fetch_directory(use_cache = TRUE)
  # Second call should use cache
  dir2 <- fetch_directory(use_cache = TRUE)
  expect_equal(nrow(dir1), nrow(dir2))

  # Clean up
  clear_directory_cache()
})
