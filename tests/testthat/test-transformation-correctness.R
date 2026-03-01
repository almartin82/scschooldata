# ==============================================================================
# Transformation Correctness Tests for scschooldata
# ==============================================================================
#
# These tests verify that every transformation step in the pipeline preserves
# data integrity, produces correct output, and handles edge cases properly.
#
# Test categories:
# 1. safe_numeric() edge cases
# 2. process_enr() schema and state aggregate correctness
# 3. tidy_enr() fidelity: wide counts == tidy counts
# 4. id_enr_aggs() flag logic
# 5. enr_grade_aggs() arithmetic
# 6. Percentage computation correctness
# 7. District ID format consistency
# 8. Multi-year data consistency
# 9. Data quality constraints (no Inf/NaN, non-negative, no duplicates)
#
# ==============================================================================

library(testthat)
library(dplyr)

# Helper: skip test if SC DOE (ed.sc.gov) is unreachable or returning errors.
skip_if_sc_doe_unavailable <- function(e) {
  if (grepl("Failed to download|too small|HTTP error|SSL|timeout|connection|Could not resolve",
            e$message, ignore.case = TRUE)) {
    skip(paste("SC DOE (ed.sc.gov) unavailable:", e$message))
  }
  stop(e)
}

# Load data once for all tests (via cache)
get_test_data <- function() {
  list(
    wide = tryCatch(
      fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
      error = skip_if_sc_doe_unavailable
    ),
    tidy = tryCatch(
      fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
      error = skip_if_sc_doe_unavailable
    )
  )
}


# ==============================================================================
# 1. safe_numeric() edge cases
# ==============================================================================

test_that("safe_numeric handles suppression markers correctly", {
  # Standard markers

  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric("-")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("n/a")))
  expect_true(is.na(safe_numeric("NA")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("N<10")))
})

test_that("safe_numeric handles commas in large numbers", {
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("1,234,567"), 1234567)
  expect_equal(safe_numeric("100,000"), 100000)
})

test_that("safe_numeric handles whitespace", {
  expect_equal(safe_numeric("  100  "), 100)
  expect_equal(safe_numeric(" 1,234 "), 1234)
  expect_equal(safe_numeric("\t50\t"), 50)
})

test_that("safe_numeric handles normal numbers", {
  expect_equal(safe_numeric("0"), 0)
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("99999"), 99999)
  expect_equal(safe_numeric(42), 42)
})

test_that("safe_numeric handles vector input", {
  result <- safe_numeric(c("100", "200", "*", "300", "N/A"))
  expect_equal(result[1], 100)
  expect_equal(result[2], 200)
  expect_true(is.na(result[3]))
  expect_equal(result[4], 300)
  expect_true(is.na(result[5]))
})


# ==============================================================================
# 2. process_enr() schema and state aggregate correctness
# ==============================================================================

test_that("process_enr produces all three entity levels", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  expect_true("State" %in% wide$type)
  expect_true("District" %in% wide$type)
  expect_true("Campus" %in% wide$type)

  # Exactly one state row
  expect_equal(sum(wide$type == "State"), 1)
})

test_that("state aggregate equals sum of districts", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  state_row <- wide[wide$type == "State", ]
  district_rows <- wide[wide$type == "District", ]

  # row_total must match

  expect_equal(
    state_row$row_total,
    sum(district_rows$row_total, na.rm = TRUE),
    info = "State row_total must equal sum of district row_totals"
  )

  # Grade columns must match
  grade_cols <- grep("^grade_", names(wide), value = TRUE)
  for (g in grade_cols) {
    expect_equal(
      state_row[[g]],
      sum(district_rows[[g]], na.rm = TRUE),
      info = paste("State", g, "must equal sum of district", g)
    )
  }

  # Demographic columns must match
  demo_cols <- c("white", "black", "hispanic", "asian",
                 "native_american", "pacific_islander", "multiracial",
                 "female", "male")
  for (dc in demo_cols) {
    if (dc %in% names(wide)) {
      expect_equal(
        state_row[[dc]],
        sum(district_rows[[dc]], na.rm = TRUE),
        info = paste("State", dc, "must equal sum of district", dc)
      )
    }
  }
})

test_that("campus enrollment sums equal state total", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  state_total <- wide$row_total[wide$type == "State"]
  campus_sum <- sum(wide$row_total[wide$type == "Campus"], na.rm = TRUE)

  expect_equal(campus_sum, state_total,
    info = "Sum of campus row_totals must equal state row_total"
  )
})

test_that("wide schema contains all required columns", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  required_cols <- c(
    "end_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "row_total",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "female", "male",
    "econ_disadv", "lep", "special_ed",
    "charter_flag", "county", "region"
  )

  for (col in required_cols) {
    expect_true(col %in% names(wide),
      info = paste("Required column missing:", col)
    )
  }
})

test_that("grade columns sum equals row_total for each entity", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  grade_cols <- grep("^grade_", names(wide), value = TRUE)

  # For each row, sum of grades should equal row_total
  grade_sums <- rowSums(wide[, grade_cols], na.rm = TRUE)

  # Only check rows where row_total is not NA and grade_sums > 0
  valid_rows <- !is.na(wide$row_total) & grade_sums > 0
  expect_true(
    all(abs(grade_sums[valid_rows] - wide$row_total[valid_rows]) < 0.01),
    info = "Sum of grade columns must equal row_total for each entity"
  )
})


# ==============================================================================
# 3. tidy_enr() fidelity: wide counts == tidy counts
# ==============================================================================

test_that("tidy total_enrollment matches wide row_total for every entity", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide
  tidy <- d$tidy

  # State level
  state_wide <- wide$row_total[wide$type == "State"]
  state_tidy <- tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)
  expect_equal(state_tidy, state_wide,
    info = "State total_enrollment in tidy must match wide row_total"
  )

  # District level - merge by district_id
  dist_wide <- wide %>%
    filter(type == "District") %>%
    select(district_id, row_total)
  dist_tidy <- tidy %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    select(district_id, n_students)

  joined <- merge(dist_wide, dist_tidy, by = "district_id")
  expect_true(
    all(joined$row_total == joined$n_students),
    info = "Every district total_enrollment in tidy must match wide row_total"
  )
  expect_equal(nrow(joined), nrow(dist_wide),
    info = "All districts in wide must appear in tidy"
  )

  # Campus level - spot check first 20 campuses
  campus_ids <- wide$campus_id[wide$type == "Campus"][1:20]
  campus_ids <- campus_ids[!is.na(campus_ids)]

  for (cid in campus_ids) {
    wide_total <- wide$row_total[wide$campus_id == cid & !is.na(wide$campus_id)]
    tidy_total <- tidy %>%
      filter(campus_id == cid, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
      pull(n_students)
    expect_equal(tidy_total, wide_total,
      info = paste("Campus", cid, "total_enrollment mismatch")
    )
  }
})

test_that("tidy demographic counts match wide columns for every entity", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide
  tidy <- d$tidy

  demo_subgroups <- c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial",
                      "female", "male", "econ_disadv")

  # Check state level for each demographic
  state_wide <- wide[wide$type == "State", ]
  for (sg in demo_subgroups) {
    if (sg %in% names(state_wide)) {
      wide_val <- state_wide[[sg]]
      tidy_val <- tidy %>%
        filter(is_state, subgroup == sg, grade_level == "TOTAL") %>%
        pull(n_students)
      expect_equal(tidy_val, wide_val,
        info = paste("State", sg, "mismatch: tidy", tidy_val, "!= wide", wide_val)
      )
    }
  }
})

test_that("tidy grade counts match wide grade columns", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide
  tidy <- d$tidy

  # Grade level mapping
  grade_map <- c(
    "grade_pk" = "PK", "grade_k" = "K",
    "grade_01" = "01", "grade_02" = "02", "grade_03" = "03",
    "grade_04" = "04", "grade_05" = "05", "grade_06" = "06",
    "grade_07" = "07", "grade_08" = "08", "grade_09" = "09",
    "grade_10" = "10", "grade_11" = "11", "grade_12" = "12"
  )

  state_wide <- wide[wide$type == "State", ]

  for (wide_col in names(grade_map)) {
    tidy_grade <- grade_map[wide_col]
    wide_val <- state_wide[[wide_col]]
    tidy_val <- tidy %>%
      filter(is_state, subgroup == "total_enrollment", grade_level == tidy_grade) %>%
      pull(n_students)

    expect_equal(tidy_val, wide_val,
      info = paste("State grade", tidy_grade, "mismatch: tidy", tidy_val, "!= wide", wide_val)
    )
  }
})

test_that("tidy sum of grade-level n_students equals TOTAL n_students", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  # For state
  state_grades <- tidy %>%
    filter(is_state, subgroup == "total_enrollment",
           grade_level %in% c("PK", "K", paste0("0", 1:9), "10", "11", "12"))
  state_total <- tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(
    sum(state_grades$n_students),
    state_total$n_students,
    info = "State: sum of individual grade n_students must equal TOTAL n_students"
  )
})

test_that("tidy_enr removes NA n_students rows", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  na_count <- sum(is.na(tidy$n_students))
  expect_equal(na_count, 0,
    info = "tidy_enr should filter out rows where n_students is NA"
  )
})

test_that("tidy has expected subgroups", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expected_subgroups <- c(
    "total_enrollment", "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "female", "male", "econ_disadv"
  )

  actual_subgroups <- unique(tidy$subgroup)
  for (sg in expected_subgroups) {
    expect_true(sg %in% actual_subgroups,
      info = paste("Missing expected subgroup:", sg)
    )
  }
})

test_that("tidy has expected grade levels", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expected_grades <- c("TOTAL", "PK", "K",
                       "01", "02", "03", "04", "05", "06",
                       "07", "08", "09", "10", "11", "12")

  actual_grades <- unique(tidy$grade_level)
  for (gl in expected_grades) {
    expect_true(gl %in% actual_grades,
      info = paste("Missing expected grade_level:", gl)
    )
  }
})


# ==============================================================================
# 4. id_enr_aggs() flag logic
# ==============================================================================

test_that("entity flags are mutually exclusive", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  # Each row should have exactly one TRUE among is_state, is_district, is_campus
  flag_sum <- tidy$is_state + tidy$is_district + tidy$is_campus
  expect_true(
    all(flag_sum == 1),
    info = "Each row must have exactly one of is_state/is_district/is_campus TRUE"
  )
})

test_that("is_state TRUE only for State type", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expect_true(all(tidy$is_state == (tidy$type == "State")))
})

test_that("is_district TRUE only for District type", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expect_true(all(tidy$is_district == (tidy$type == "District")))
})

test_that("is_campus TRUE only for Campus type", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expect_true(all(tidy$is_campus == (tidy$type == "Campus")))
})

test_that("aggregation_flag is consistent with type", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expect_true(all(tidy$aggregation_flag[tidy$type == "State"] == "state"))
  expect_true(all(tidy$aggregation_flag[tidy$type == "District"] == "district"))
  expect_true(all(tidy$aggregation_flag[tidy$type == "Campus"] == "campus"))
})

test_that("is_charter is logical", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expect_true(is.logical(tidy$is_charter))
})


# ==============================================================================
# 5. enr_grade_aggs() arithmetic
# ==============================================================================

test_that("K8 aggregate is sum of K through 08", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy
  aggs <- enr_grade_aggs(tidy)

  # State level
  k8_from_agg <- aggs %>%
    filter(is_state, grade_level == "K8") %>%
    pull(n_students)

  k8_manual <- tidy %>%
    filter(is_state, subgroup == "total_enrollment",
           grade_level %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08")) %>%
    summarise(total = sum(n_students)) %>%
    pull(total)

  expect_equal(k8_from_agg, k8_manual,
    info = "K8 aggregate must equal sum of K through 08"
  )
})

test_that("HS aggregate is sum of 09 through 12", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy
  aggs <- enr_grade_aggs(tidy)

  hs_from_agg <- aggs %>%
    filter(is_state, grade_level == "HS") %>%
    pull(n_students)

  hs_manual <- tidy %>%
    filter(is_state, subgroup == "total_enrollment",
           grade_level %in% c("09", "10", "11", "12")) %>%
    summarise(total = sum(n_students)) %>%
    pull(total)

  expect_equal(hs_from_agg, hs_manual,
    info = "HS aggregate must equal sum of 09 through 12"
  )
})

test_that("K12 equals K8 + HS", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy
  aggs <- enr_grade_aggs(tidy)

  state_aggs <- aggs %>% filter(is_state)
  k8 <- state_aggs$n_students[state_aggs$grade_level == "K8"]
  hs <- state_aggs$n_students[state_aggs$grade_level == "HS"]
  k12 <- state_aggs$n_students[state_aggs$grade_level == "K12"]

  expect_equal(k12, k8 + hs,
    info = "K12 must equal K8 + HS"
  )
})

test_that("K12 excludes PK and EE", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy
  aggs <- enr_grade_aggs(tidy)

  state_k12 <- aggs %>%
    filter(is_state, grade_level == "K12") %>%
    pull(n_students)

  state_total <- tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  # K12 should be strictly less than TOTAL if PK exists
  pk_count <- tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "PK") %>%
    pull(n_students)

  if (length(pk_count) > 0 && pk_count > 0) {
    expect_true(state_k12 < state_total,
      info = "K12 should be less than TOTAL when PK enrollment exists"
    )
    # TOTAL should approximately equal K12 + PK (+ EE if present)
    ee_count <- tidy %>%
      filter(is_state, subgroup == "total_enrollment", grade_level == "EE") %>%
      pull(n_students)
    ee_val <- if (length(ee_count) > 0) ee_count else 0

    expect_equal(state_total, state_k12 + pk_count + ee_val,
      info = "TOTAL should equal K12 + PK + EE"
    )
  }
})

test_that("enr_grade_aggs produces aggregates for all entity types", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy
  aggs <- enr_grade_aggs(tidy)

  # Should have state, district, and campus level aggregates
  expect_true(any(aggs$is_state), info = "Should have state-level aggregates")
  expect_true(any(aggs$is_district), info = "Should have district-level aggregates")
  expect_true(any(aggs$is_campus), info = "Should have campus-level aggregates")

  # Each entity should have K8, HS, K12
  for (gl in c("K8", "HS", "K12")) {
    expect_true(gl %in% aggs$grade_level,
      info = paste("Missing grade_level aggregate:", gl)
    )
  }
})

test_that("enr_grade_aggs only operates on total_enrollment subgroup", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy
  aggs <- enr_grade_aggs(tidy)

  expect_true(
    all(aggs$subgroup == "total_enrollment"),
    info = "Grade aggregates should only be for total_enrollment subgroup"
  )
})


# ==============================================================================
# 6. Percentage computation correctness
# ==============================================================================

test_that("pct for total_enrollment is always 1.0 at TOTAL grade level", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  te_total <- tidy %>%
    filter(subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_true(
    all(te_total$pct == 1.0),
    info = "total_enrollment at TOTAL grade_level must always have pct == 1.0"
  )
})

test_that("pct equals n_students divided by row_total (via wide)", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide
  tidy <- d$tidy

  # Spot-check 5 campuses
  campus_ids <- wide$campus_id[wide$type == "Campus"][c(1, 50, 100, 200, 500)]
  campus_ids <- campus_ids[!is.na(campus_ids)]

  for (cid in campus_ids) {
    wide_row <- wide[wide$campus_id == cid & !is.na(wide$campus_id), ]
    if (nrow(wide_row) == 0 || is.na(wide_row$row_total) || wide_row$row_total == 0) next

    for (sg in c("white", "black", "hispanic")) {
      if (is.na(wide_row[[sg]])) next
      expected_pct <- wide_row[[sg]] / wide_row$row_total

      tidy_row <- tidy %>%
        filter(campus_id == cid, subgroup == sg, grade_level == "TOTAL")

      if (nrow(tidy_row) > 0) {
        expect_equal(tidy_row$pct, expected_pct, tolerance = 1e-10,
          info = paste("Campus", cid, "subgroup", sg, "pct mismatch")
        )
      }
    }
  }
})

test_that("pct is between 0 and 1 for all demographic subgroups", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  demo_rows <- tidy %>%
    filter(subgroup != "total_enrollment", !is.na(pct))

  expect_true(
    all(demo_rows$pct >= 0),
    info = "pct must be >= 0 for all demographic subgroups"
  )
  expect_true(
    all(demo_rows$pct <= 1),
    info = "pct must be <= 1 for all demographic subgroups"
  )
})

test_that("grade-level pct equals n_students divided by total enrollment", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide
  tidy <- d$tidy

  # Check state level
  state_wide <- wide[wide$type == "State", ]
  grade_map <- c(
    "grade_01" = "01", "grade_05" = "05", "grade_09" = "09", "grade_12" = "12"
  )

  for (wcol in names(grade_map)) {
    tidy_gl <- grade_map[wcol]
    expected_pct <- state_wide[[wcol]] / state_wide$row_total

    tidy_row <- tidy %>%
      filter(is_state, subgroup == "total_enrollment", grade_level == tidy_gl)

    expect_equal(tidy_row$pct, expected_pct, tolerance = 1e-10,
      info = paste("State grade", tidy_gl, "pct mismatch")
    )
  }
})


# ==============================================================================
# 7. District ID format consistency
# ==============================================================================

test_that("district_id format is consistent within entity types", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  district_rows <- wide[wide$type == "District", ]
  campus_rows <- wide[wide$type == "Campus", ]

  # All district IDs should be non-NA for District rows
  expect_true(
    all(!is.na(district_rows$district_id)),
    info = "District rows must all have non-NA district_id"
  )

  # All campus IDs should be non-NA for Campus rows
  expect_true(
    all(!is.na(campus_rows$campus_id)),
    info = "Campus rows must all have non-NA campus_id"
  )

  # Campus district_id should be non-NA
  expect_true(
    all(!is.na(campus_rows$district_id)),
    info = "Campus rows must all have non-NA district_id"
  )
})

test_that("campus_id is 7 digits", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  campus_ids <- wide$campus_id[wide$type == "Campus"]
  campus_ids <- campus_ids[!is.na(campus_ids)]

  expect_true(
    all(grepl("^\\d{7}$", campus_ids)),
    info = "All campus_id values must be 7-digit strings"
  )
})

test_that("state row has NA district_id and campus_id", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  state_row <- wide[wide$type == "State", ]
  expect_true(is.na(state_row$district_id))
  expect_true(is.na(state_row$campus_id))
})

test_that("district rows have NA campus_id", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  district_rows <- wide[wide$type == "District", ]
  expect_true(
    all(is.na(district_rows$campus_id)),
    info = "District rows must have NA campus_id"
  )
  expect_true(
    all(is.na(district_rows$campus_name)),
    info = "District rows must have NA campus_name"
  )
})


# ==============================================================================
# 8. Multi-year data consistency
# ==============================================================================

test_that("fetch_enr_multi combines years correctly", {
  skip_on_cran()
  skip_if_offline()

  multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Both years present
  expect_true(2023 %in% multi$end_year)
  expect_true(2024 %in% multi$end_year)

  # Each year has state total
  state_totals <- multi %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(state_totals), 2)

  # State totals are reasonable (SC has ~750k-800k students)
  expect_true(all(state_totals$n_students > 500000),
    info = "State totals must be > 500,000 (SC has ~750k students)"
  )
  expect_true(all(state_totals$n_students < 1500000),
    info = "State totals must be < 1,500,000"
  )
})

test_that("multi-year data matches individual year fetches", {
  skip_on_cran()
  skip_if_offline()

  multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  single_23 <- tryCatch(
    fetch_enr(2023, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # State total for 2023 should match
  multi_st_23 <- multi %>%
    filter(end_year == 2023, is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  single_st_23 <- single_23 %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(multi_st_23, single_st_23,
    info = "Multi-year 2023 state total must match single-year 2023 fetch"
  )
})

test_that("multi-year data has consistent columns across years", {
  skip_on_cran()
  skip_if_offline()

  multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Check that both years have same subgroups
  sg_23 <- sort(unique(multi$subgroup[multi$end_year == 2023]))
  sg_24 <- sort(unique(multi$subgroup[multi$end_year == 2024]))
  expect_equal(sg_23, sg_24,
    info = "Subgroups should be consistent across years"
  )

  # Same grade levels
  gl_23 <- sort(unique(multi$grade_level[multi$end_year == 2023]))
  gl_24 <- sort(unique(multi$grade_level[multi$end_year == 2024]))
  expect_equal(gl_23, gl_24,
    info = "Grade levels should be consistent across years"
  )
})


# ==============================================================================
# 9. Data quality constraints
# ==============================================================================

test_that("no Inf values in numeric columns", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  numeric_cols <- names(tidy)[sapply(tidy, is.numeric)]
  for (col in numeric_cols) {
    expect_false(
      any(is.infinite(tidy[[col]]), na.rm = TRUE),
      info = paste("Column", col, "must not contain Inf values")
    )
  }
})

test_that("no NaN values in numeric columns", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  numeric_cols <- names(tidy)[sapply(tidy, is.numeric)]
  for (col in numeric_cols) {
    expect_false(
      any(is.nan(tidy[[col]]), na.rm = TRUE),
      info = paste("Column", col, "must not contain NaN values")
    )
  }
})

test_that("n_students is non-negative", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expect_true(
    all(tidy$n_students >= 0, na.rm = TRUE),
    info = "n_students must be non-negative"
  )
})

test_that("no duplicate rows in tidy data", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  key_cols <- c("end_year", "type", "district_id", "campus_id", "subgroup", "grade_level")
  dups <- tidy %>%
    count(across(all_of(key_cols))) %>%
    filter(n > 1)

  expect_equal(nrow(dups), 0,
    info = "No duplicate rows allowed in tidy data (by year/type/district/campus/subgroup/grade)"
  )
})

test_that("end_year is within valid range", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  expect_true(all(tidy$end_year >= 2013 & tidy$end_year <= 2026),
    info = "end_year must be between 2013 and 2026"
  )
})

test_that("SC has a reasonable number of districts (75-90)", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  n_districts <- sum(wide$type == "District")
  expect_true(n_districts >= 75 & n_districts <= 95,
    info = paste("SC should have 75-90 districts, found", n_districts)
  )
})

test_that("SC has a reasonable number of campuses (1000-1500)", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  wide <- d$wide

  n_campuses <- sum(wide$type == "Campus")
  expect_true(n_campuses >= 1000 & n_campuses <= 1500,
    info = paste("SC should have 1000-1500 campuses, found", n_campuses)
  )
})


# ==============================================================================
# 10. Input validation
# ==============================================================================

test_that("fetch_enr rejects invalid years", {
  expect_error(fetch_enr(2012), "end_year must be between")
  expect_error(fetch_enr(2027), "end_year must be between")
  expect_error(fetch_enr(1990), "end_year must be between")
  expect_error(fetch_enr(2050), "end_year must be between")
})

test_that("fetch_enr rejects invalid count_day", {
  expect_error(fetch_enr(2024, count_day = "50"), "count_day must be")
  expect_error(fetch_enr(2024, count_day = "0"), "count_day must be")
  expect_error(fetch_enr(2024, count_day = "abc"), "count_day must be")
})

test_that("fetch_enr_multi rejects invalid years", {
  expect_error(fetch_enr_multi(c(2012, 2024)), "Invalid years")
  expect_error(fetch_enr_multi(c(2024, 2027)), "Invalid years")
})

test_that("get_available_years validates source parameter", {
  expect_error(get_available_years("invalid"))
})

test_that("fetch_report_cards rejects invalid years", {
  expect_error(fetch_report_cards(2017), "available from 2018")
  expect_error(fetch_report_cards(2026), "available from 2018")
})


# ==============================================================================
# 11. clean_text() edge cases
# ==============================================================================

test_that("clean_text handles all line break types", {
  expect_equal(clean_text("a\r\nb"), "a b")
  expect_equal(clean_text("a\rb"), "a b")
  expect_equal(clean_text("a\nb"), "a b")
  expect_equal(clean_text("a\r\n\r\nb"), "a b")
})

test_that("clean_text collapses multiple spaces", {
  expect_equal(clean_text("a    b"), "a b")
  expect_equal(clean_text("  a  b  c  "), "a b c")
})

test_that("clean_text trims leading and trailing whitespace", {
  expect_equal(clean_text("  hello  "), "hello")
  expect_equal(clean_text("\thello\t"), "hello")
})


# ==============================================================================
# 12. Column name generation functions
# ==============================================================================

test_that("get_grade_column_names school has school_id and 16 grade columns", {
  cols <- get_grade_column_names("school")
  expect_true("school_id" %in% cols)
  expect_true("district_name" %in% cols)
  expect_true("school_name" %in% cols)
  expect_true("total" %in% cols)
  expect_true("grade_pk" %in% cols)
  expect_true("grade_k" %in% cols)
  expect_true("grade_12" %in% cols)

  # Should have PK, K, 01-12 = 14 grade columns + id + district + school + total = 18
  expect_equal(length(cols), 18)
})

test_that("get_grade_column_names district has district_id and no school_id", {
  cols <- get_grade_column_names("district")
  expect_true("district_id" %in% cols)
  expect_true("district_name" %in% cols)
  expect_false("school_id" %in% cols)
  expect_false("school_name" %in% cols)
  expect_true("grade_01" %in% cols)

  # district_id + name + total + 14 grades = 17
  expect_equal(length(cols), 17)
})

test_that("get_demo_column_names school has all demographic columns", {
  cols <- get_demo_column_names("school")
  expect_true("school_id" %in% cols)
  expect_true("female" %in% cols)
  expect_true("male" %in% cols)
  expect_true("white" %in% cols)
  expect_true("black" %in% cols)
  expect_true("hispanic" %in% cols)
  expect_true("asian" %in% cols)
  expect_true("native_american" %in% cols)
  expect_true("pacific_islander" %in% cols)
  expect_true("multiracial" %in% cols)
  expect_true("econ_disadv" %in% cols)
  expect_true("not_econ_disadv" %in% cols)
})

test_that("get_demo_column_names district has district_id", {
  cols <- get_demo_column_names("district")
  expect_true("district_id" %in% cols)
  expect_false("school_id" %in% cols)
  expect_true("econ_disadv" %in% cols)
})


# ==============================================================================
# 13. Race demographic sum check
# ==============================================================================

test_that("race subgroup counts approximate row_total (allowing for gap)", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  # At state level, sum of race subgroups should be close to total
  race_subgroups <- c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial")

  state_races <- tidy %>%
    filter(is_state, grade_level == "TOTAL", subgroup %in% race_subgroups)

  state_total <- tidy %>%
    filter(is_state, grade_level == "TOTAL", subgroup == "total_enrollment") %>%
    pull(n_students)

  race_sum <- sum(state_races$n_students)

  # Allow some gap for "missing" or "other" race category
  gap_pct <- abs(state_total - race_sum) / state_total
  expect_true(gap_pct < 0.01,
    info = paste("Race sum gap should be < 1% of total. Gap:", round(gap_pct * 100, 2), "%")
  )
})

test_that("gender subgroup counts approximate row_total", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  state_genders <- tidy %>%
    filter(is_state, grade_level == "TOTAL", subgroup %in% c("male", "female"))

  state_total <- tidy %>%
    filter(is_state, grade_level == "TOTAL", subgroup == "total_enrollment") %>%
    pull(n_students)

  gender_sum <- sum(state_genders$n_students)

  # Allow small gap for gender_missing
  gap_pct <- abs(state_total - gender_sum) / state_total
  expect_true(gap_pct < 0.01,
    info = paste("Gender sum gap should be < 1% of total. Gap:", round(gap_pct * 100, 2), "%")
  )
})


# ==============================================================================
# 14. Tidy-only fields consistency
# ==============================================================================

test_that("district_name is present for all district and campus rows", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  dist_campus <- tidy %>% filter(type %in% c("District", "Campus"))
  na_names <- sum(is.na(dist_campus$district_name))
  expect_equal(na_names, 0,
    info = "district_name should not be NA for district or campus rows"
  )
})

test_that("campus_name is present for all campus rows", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  campus_rows <- tidy %>% filter(type == "Campus")
  na_names <- sum(is.na(campus_rows$campus_name))
  expect_equal(na_names, 0,
    info = "campus_name should not be NA for campus rows"
  )
})

test_that("state row has NA for name and ID fields", {
  skip_on_cran()
  skip_if_offline()

  d <- get_test_data()
  tidy <- d$tidy

  state_rows <- tidy %>% filter(is_state)
  expect_true(all(is.na(state_rows$district_id)))
  expect_true(all(is.na(state_rows$campus_id)))
  expect_true(all(is.na(state_rows$district_name)))
  expect_true(all(is.na(state_rows$campus_name)))
})
