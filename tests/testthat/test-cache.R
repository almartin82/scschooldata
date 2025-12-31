# Tests for caching functions

test_that("get_cache_dir creates directory if needed", {
  cache_dir <- get_cache_dir()
  expect_true(dir.exists(cache_dir))
})

test_that("get_cache_path generates correct paths", {
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024\\.rds$", path))

  path_wide <- get_cache_path(2023, "wide")
  expect_true(grepl("enr_wide_2023\\.rds$", path_wide))
})

test_that("cache_exists returns FALSE for non-existent files", {
  expect_false(cache_exists(1900, "tidy"))
  expect_false(cache_exists(9999, "wide"))
})

test_that("cache read/write roundtrip works", {
  # Create test data
  test_df <- data.frame(
    x = 1:5,
    y = letters[1:5],
    stringsAsFactors = FALSE
  )

  # Write to cache (use year 9998 to avoid conflicts)
  write_cache(test_df, 9998, "test")

  # Verify it exists
  expect_true(cache_exists(9998, "test", max_age = 1))

  # Read it back
  read_df <- read_cache(9998, "test")
  expect_equal(test_df, read_df)

  # Clean up
  clear_cache(9998, "test")
  expect_false(cache_exists(9998, "test"))
})

test_that("clear_cache removes files", {
  # Create multiple test files
  write_cache(data.frame(a = 1), 9997, "test1")
  write_cache(data.frame(a = 2), 9997, "test2")
  write_cache(data.frame(a = 3), 9996, "test1")

  # Clear specific file
  clear_cache(9997, "test1")
  expect_false(cache_exists(9997, "test1"))
  expect_true(cache_exists(9997, "test2"))

  # Clear by year
  clear_cache(9997)
  expect_false(cache_exists(9997, "test2"))

  # Clean up remaining
  clear_cache(9996)
})

test_that("cache_status returns data frame", {
  # This just tests that the function runs without error
  result <- cache_status()
  expect_true(is.data.frame(result))
})
