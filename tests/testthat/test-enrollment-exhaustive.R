# ==============================================================================
# Exhaustive Tests for SC Enrollment Functions
# ==============================================================================
#
# Tests EVERY exported enrollment function with ALL parameter combinations.
# Pinned values come from real SC DOE data (2024 = 2023-24 school year).
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
# get_available_years() — exhaustive parameter tests
# ==============================================================================

test_that("get_available_years('headcounts') returns 2013:2026", {
  years <- get_available_years("headcounts")
  expect_identical(years, 2013:2026)
  expect_length(years, 14)
  expect_equal(min(years), 2013L)
  expect_equal(max(years), 2026L)
})

test_that("get_available_years('reportcards') returns 2018:2025", {
  years <- get_available_years("reportcards")
  expect_identical(years, 2018:2025)
  expect_length(years, 8)
  expect_equal(min(years), 2018L)
  expect_equal(max(years), 2025L)
})

test_that("get_available_years() defaults to headcounts", {
  expect_identical(get_available_years(), get_available_years("headcounts"))
})

test_that("get_available_years rejects invalid source", {
  expect_error(get_available_years("invalid"), "should be one of")
})


# ==============================================================================
# fetch_enr() — parameter validation tests
# ==============================================================================

test_that("fetch_enr rejects years below 2013", {
  expect_error(fetch_enr(2012, use_cache = TRUE), "end_year must be between")
  expect_error(fetch_enr(2000, use_cache = TRUE), "end_year must be between")
  expect_error(fetch_enr(1990, use_cache = TRUE), "end_year must be between")
})

test_that("fetch_enr rejects years above 2026", {
  expect_error(fetch_enr(2027, use_cache = TRUE), "end_year must be between")
  expect_error(fetch_enr(2030, use_cache = TRUE), "end_year must be between")
  expect_error(fetch_enr(3000, use_cache = TRUE), "end_year must be between")
})

test_that("fetch_enr rejects invalid count_day", {
  expect_error(fetch_enr(2024, count_day = "50", use_cache = TRUE), "count_day must be")
  expect_error(fetch_enr(2024, count_day = "90", use_cache = TRUE), "count_day must be")
  expect_error(fetch_enr(2024, count_day = "day45", use_cache = TRUE), "count_day must be")
})

test_that("fetch_enr accepts valid count_day values", {
  # These should not error on the count_day validation itself
  # (they may error on network if not cached, but not on validation)
  expect_no_error(tryCatch(
    fetch_enr(2024, count_day = "45", use_cache = TRUE),
    error = function(e) {
      if (!grepl("count_day must be", e$message)) invisible(NULL)
      else stop(e)
    }
  ))
})

# ==============================================================================
# fetch_enr_multi() — parameter validation tests
# ==============================================================================

test_that("fetch_enr_multi rejects invalid years", {
  expect_error(fetch_enr_multi(2010:2013, use_cache = TRUE), "Invalid years")
  expect_error(fetch_enr_multi(c(2024, 2030), use_cache = TRUE), "Invalid years")
  expect_error(fetch_enr_multi(c(2000, 2024), use_cache = TRUE), "Invalid years")
})

test_that("fetch_report_cards rejects invalid years", {
  expect_error(fetch_report_cards(2017, use_cache = TRUE), "Report Cards data available")
  expect_error(fetch_report_cards(2026, use_cache = TRUE), "Report Cards data available")
  expect_error(fetch_report_cards(2010, use_cache = TRUE), "Report Cards data available")
})


# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — wide output structure tests
# ==============================================================================

test_that("fetch_enr wide format has correct structure", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(is.data.frame(enr_wide))

  # Exact column names
  expected_cols <- c(
    "end_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "county", "region", "charter_flag",
    "row_total", "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "econ_disadv", "lep", "special_ed", "female", "male",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(enr_wide), info = paste("Missing column:", col))
  }
  expect_equal(ncol(enr_wide), 36)
})

test_that("fetch_enr wide format has correct row counts", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Pinned: 1 state + 82 districts + 1220 campuses = 1303
  expect_equal(nrow(enr_wide), 1303)
  expect_equal(sum(enr_wide$type == "State"), 1)
  expect_equal(sum(enr_wide$type == "District"), 82)
  expect_equal(sum(enr_wide$type == "Campus"), 1220)
})

test_that("fetch_enr wide format pinned state enrollment (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state <- enr_wide[enr_wide$type == "State", ]

  # Pinned values from real SC DOE data (2023-24 school year)
  expect_equal(state$row_total, 793551)
  expect_equal(state$white, 374275)
  expect_equal(state$black, 247301)
  expect_equal(state$hispanic, 106071)
  expect_equal(state$asian, 14635)
  expect_equal(state$native_american, 2312)
  expect_equal(state$pacific_islander, 948)
  expect_equal(state$multiracial, 47909)
  expect_equal(state$female, 387710)
  expect_equal(state$male, 405810)
  expect_equal(state$econ_disadv, 100)
})

test_that("fetch_enr wide format pinned grade counts (2024 state)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state <- enr_wide[enr_wide$type == "State", ]

  expect_equal(state$grade_pk, 27463)
  expect_equal(state$grade_k, 54461)
  expect_equal(state$grade_01, 56888)
  expect_equal(state$grade_02, 58255)
  expect_equal(state$grade_03, 57431)
  expect_equal(state$grade_04, 57857)
  expect_equal(state$grade_05, 58014)
  expect_equal(state$grade_06, 59125)
  expect_equal(state$grade_07, 59655)
  expect_equal(state$grade_08, 60348)
  expect_equal(state$grade_09, 71648)
  expect_equal(state$grade_10, 63649)
  expect_equal(state$grade_11, 56889)
  expect_equal(state$grade_12, 51868)
})

test_that("fetch_enr wide grade sum equals row_total", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state <- enr_wide[enr_wide$type == "State", ]
  grade_cols <- c("grade_pk", "grade_k", paste0("grade_", sprintf("%02d", 1:12)))
  grade_sum <- sum(state[, grade_cols], na.rm = TRUE)
  expect_equal(grade_sum, state$row_total)
})

test_that("fetch_enr wide district sum equals state total", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_total <- enr_wide$row_total[enr_wide$type == "State"]
  district_sum <- sum(enr_wide$row_total[enr_wide$type == "District"], na.rm = TRUE)
  expect_equal(district_sum, state_total)
})

test_that("fetch_enr wide pinned Greenville district (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  gv <- enr_wide[enr_wide$district_id == "2301" & enr_wide$type == "District", ]
  expect_equal(nrow(gv), 1)
  expect_equal(gv$district_name, "Greenville 01")
  expect_equal(gv$row_total, 78364)
  expect_equal(gv$grade_k, 5417)
  expect_equal(gv$grade_01, 5946)
  expect_equal(gv$grade_09, 6777)
  expect_equal(gv$grade_12, 5224)
})

test_that("fetch_enr wide pinned Charleston district (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  chs <- enr_wide[enr_wide$district_id == "1001" & enr_wide$type == "District", ]
  expect_equal(nrow(chs), 1)
  expect_equal(chs$district_name, "Charleston 01")
  expect_equal(chs$row_total, 50398)
})

test_that("fetch_enr wide pinned Wando High School (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  wando <- enr_wide[which(enr_wide$campus_id == "1001014"), ]
  expect_equal(nrow(wando), 1)
  expect_equal(wando$campus_name, "Wando High")
  expect_equal(wando$district_name, "Charleston 01")
  expect_equal(wando$row_total, 2592)
  expect_equal(wando$white, 2097)
  expect_equal(wando$black, 231)
  expect_equal(wando$hispanic, 121)
  expect_equal(wando$asian, 63)
  expect_equal(wando$male, 1246)
  expect_equal(wando$female, 1346)
})

test_that("fetch_enr wide charter districts present (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # SC has charter school districts
  charter <- enr_wide[grepl("Charter", enr_wide$district_name, ignore.case = TRUE) &
                         enr_wide$type == "District", ]
  expect_true(nrow(charter) >= 1)

  # SC Public Charter School District
  sc_charter <- enr_wide[enr_wide$district_id == "4701" & enr_wide$type == "District", ]
  expect_equal(nrow(sc_charter), 1)
  expect_equal(sc_charter$row_total, 18404)
})

test_that("fetch_enr wide lep and special_ed are zero/NA at all levels", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # lep and special_ed columns are either 0 or NA in SC data
  lep_vals <- enr_wide$lep[!is.na(enr_wide$lep)]
  sped_vals <- enr_wide$special_ed[!is.na(enr_wide$special_ed)]

  expect_true(all(lep_vals == 0))
  expect_true(all(sped_vals == 0))
})

test_that("fetch_enr wide county and region are all NA", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(all(is.na(enr_wide$county)))
  expect_true(all(is.na(enr_wide$region)))
})

test_that("fetch_enr wide charter_flag is all NA", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(all(is.na(enr_wide$charter_flag)))
})


# ==============================================================================
# fetch_enr(2024, tidy = TRUE) — tidy output structure tests
# ==============================================================================

test_that("fetch_enr tidy format has correct structure", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(is.data.frame(enr_tidy))
  expect_equal(ncol(enr_tidy), 18)

  expected_cols <- c(
    "end_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "county", "region", "charter_flag",
    "grade_level", "subgroup", "n_students", "pct",
    "is_state", "is_district", "is_campus", "is_charter", "aggregation_flag"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(enr_tidy), info = paste("Missing column:", col))
  }
})

test_that("fetch_enr tidy has correct row count (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_equal(nrow(enr_tidy), 32577)
})

test_that("fetch_enr tidy has correct entity counts (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # State rows at TOTAL
  state_total <- enr_tidy %>% filter(is_state, grade_level == "TOTAL")
  expect_equal(nrow(state_total), 13)

  # District rows at TOTAL
  dist_total <- enr_tidy %>% filter(is_district, grade_level == "TOTAL")
  expect_equal(nrow(dist_total), 902)

  # Campus rows at TOTAL
  campus_total <- enr_tidy %>% filter(is_campus, grade_level == "TOTAL")
  expect_equal(nrow(campus_total), 13420)

  # Number of distinct districts
  n_districts <- n_distinct(enr_tidy$district_id[enr_tidy$is_district])
  expect_equal(n_districts, 82)

  # Number of distinct campuses
  n_campuses <- n_distinct(enr_tidy$campus_id[enr_tidy$is_campus])
  expect_equal(n_campuses, 1220)
})

test_that("fetch_enr tidy has correct subgroups", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  subgroups <- sort(unique(enr_tidy$subgroup))
  expected <- sort(c(
    "asian", "black", "econ_disadv", "female", "hispanic",
    "lep", "male", "multiracial", "native_american",
    "pacific_islander", "special_ed", "total_enrollment", "white"
  ))
  expect_equal(subgroups, expected)
})

test_that("fetch_enr tidy has correct grade levels", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  grade_levels <- sort(unique(enr_tidy$grade_level))
  expected <- sort(c(
    "01", "02", "03", "04", "05", "06", "07", "08",
    "09", "10", "11", "12", "K", "PK", "TOTAL"
  ))
  expect_equal(grade_levels, expected)
})

test_that("fetch_enr tidy has correct types", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  types <- sort(unique(enr_tidy$type))
  expect_equal(types, sort(c("State", "District", "Campus")))
})

test_that("fetch_enr tidy pinned state enrollment (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_total <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(state_total), 1)
  expect_equal(state_total$n_students, 793551)
  expect_equal(state_total$pct, 1.0)
  expect_true(state_total$is_state)
  expect_false(state_total$is_district)
  expect_false(state_total$is_campus)
  expect_false(state_total$is_charter)
})

test_that("fetch_enr tidy pinned state demographics (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_demos <- enr_tidy %>%
    filter(is_state, grade_level == "TOTAL") %>%
    select(subgroup, n_students) %>%
    arrange(subgroup)

  # Pin each subgroup
  get_state_n <- function(sg) {
    state_demos$n_students[state_demos$subgroup == sg]
  }

  expect_equal(get_state_n("white"), 374275)
  expect_equal(get_state_n("black"), 247301)
  expect_equal(get_state_n("hispanic"), 106071)
  expect_equal(get_state_n("asian"), 14635)
  expect_equal(get_state_n("native_american"), 2312)
  expect_equal(get_state_n("pacific_islander"), 948)
  expect_equal(get_state_n("multiracial"), 47909)
  expect_equal(get_state_n("econ_disadv"), 100)
  expect_equal(get_state_n("male"), 405810)
  expect_equal(get_state_n("female"), 387710)
  expect_equal(get_state_n("lep"), 0)
  expect_equal(get_state_n("special_ed"), 0)
})

test_that("fetch_enr tidy pinned state grade levels (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_grades <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level != "TOTAL") %>%
    select(grade_level, n_students) %>%
    arrange(grade_level)

  get_grade_n <- function(gl) {
    state_grades$n_students[state_grades$grade_level == gl]
  }

  expect_equal(get_grade_n("PK"), 27463)
  expect_equal(get_grade_n("K"), 54461)
  expect_equal(get_grade_n("01"), 56888)
  expect_equal(get_grade_n("02"), 58255)
  expect_equal(get_grade_n("03"), 57431)
  expect_equal(get_grade_n("04"), 57857)
  expect_equal(get_grade_n("05"), 58014)
  expect_equal(get_grade_n("06"), 59125)
  expect_equal(get_grade_n("07"), 59655)
  expect_equal(get_grade_n("08"), 60348)
  expect_equal(get_grade_n("09"), 71648)
  expect_equal(get_grade_n("10"), 63649)
  expect_equal(get_grade_n("11"), 56889)
  expect_equal(get_grade_n("12"), 51868)
})

test_that("fetch_enr tidy grade sum = TOTAL for state (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  grade_sum <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level != "TOTAL") %>%
    summarize(total = sum(n_students)) %>%
    pull(total)

  total_row <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(grade_sum, total_row)
})

test_that("fetch_enr tidy district sum = state total (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  district_sum <- enr_tidy %>%
    filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    summarize(total = sum(n_students)) %>%
    pull(total)

  state_total <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(district_sum, state_total)
})

test_that("fetch_enr tidy pct values are valid (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # pct should be between 0 and 1 (inclusive) or NA
  valid_pct <- enr_tidy$pct[!is.na(enr_tidy$pct)]
  expect_true(all(valid_pct >= 0))
  expect_true(all(valid_pct <= 1))

  # total_enrollment at TOTAL should have pct = 1.0
  total_pcts <- enr_tidy %>%
    filter(subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(pct)
  expect_true(all(total_pcts == 1.0))
})

test_that("fetch_enr tidy racial pct sums to ~1 for state (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  racial <- enr_tidy %>%
    filter(is_state, grade_level == "TOTAL",
           subgroup %in% c("white", "black", "hispanic", "asian",
                           "native_american", "pacific_islander", "multiracial"))
  pct_sum <- sum(racial$pct, na.rm = TRUE)
  expect_true(abs(pct_sum - 1.0) < 0.01, info = paste("Racial pct sum:", pct_sum))
})

test_that("fetch_enr tidy pinned Greenville district (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  gv <- enr_tidy %>%
    filter(is_district, district_id == "2301",
           subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(gv), 1)
  expect_equal(gv$district_name, "Greenville 01")
  expect_equal(gv$n_students, 78364)
})

test_that("fetch_enr tidy pinned Greenville demographics (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  gv_demos <- enr_tidy %>%
    filter(is_district, district_id == "2301", grade_level == "TOTAL") %>%
    select(subgroup, n_students)

  get_gv_n <- function(sg) {
    gv_demos$n_students[gv_demos$subgroup == sg]
  }

  expect_equal(get_gv_n("total_enrollment"), 78364)
  expect_equal(get_gv_n("white"), 38516)
  expect_equal(get_gv_n("black"), 17500)
  expect_equal(get_gv_n("hispanic"), 16353)
  expect_equal(get_gv_n("asian"), 1885)
  expect_equal(get_gv_n("native_american"), 245)
  expect_equal(get_gv_n("pacific_islander"), 136)
  expect_equal(get_gv_n("multiracial"), 3729)
  expect_equal(get_gv_n("male"), 40166)
  expect_equal(get_gv_n("female"), 38197)
})

test_that("fetch_enr tidy pinned Wando High (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  wando <- enr_tidy %>%
    filter(is_campus, campus_id == "1001014", grade_level == "TOTAL") %>%
    select(subgroup, n_students)

  get_wando_n <- function(sg) {
    wando$n_students[wando$subgroup == sg]
  }

  expect_equal(get_wando_n("total_enrollment"), 2592)
  expect_equal(get_wando_n("white"), 2097)
  expect_equal(get_wando_n("black"), 231)
  expect_equal(get_wando_n("hispanic"), 121)
  expect_equal(get_wando_n("asian"), 63)
  expect_equal(get_wando_n("native_american"), 1)
  expect_equal(get_wando_n("pacific_islander"), 1)
  expect_equal(get_wando_n("multiracial"), 78)
  expect_equal(get_wando_n("male"), 1246)
  expect_equal(get_wando_n("female"), 1346)
})

test_that("fetch_enr tidy Horry County total (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  horry <- enr_tidy %>%
    filter(is_district, district_id == "2601",
           subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(horry), 1)
  expect_equal(horry$n_students, 48205)
})

test_that("fetch_enr tidy SC Public Charter School District (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  charter <- enr_tidy %>%
    filter(is_district, district_id == "4701",
           subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(charter), 1)
  expect_equal(charter$n_students, 18404)
  expect_equal(charter$district_name, "SC Public Charter School District")
})

test_that("fetch_enr tidy Charter Institute at Erskine (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  erskine <- enr_tidy %>%
    filter(is_district, district_id == "4801",
           subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(erskine), 1)
  expect_equal(erskine$n_students, 25146)
  expect_equal(erskine$district_name, "Charter Institute at Erskine")
})


# ==============================================================================
# fetch_enr tidy — subgroup by grade level structure tests
# ==============================================================================

test_that("demographic subgroups only appear at TOTAL grade level", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  demo_subgroups <- c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial",
                      "econ_disadv", "male", "female", "lep", "special_ed")

  for (sg in demo_subgroups) {
    rows <- enr_tidy %>% filter(subgroup == sg)
    if (nrow(rows) > 0) {
      expect_true(
        all(rows$grade_level == "TOTAL"),
        info = paste(sg, "should only appear at grade_level TOTAL")
      )
    }
  }
})

test_that("grade-level rows only have total_enrollment subgroup", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  non_total_grades <- enr_tidy %>% filter(grade_level != "TOTAL")
  expect_true(all(non_total_grades$subgroup == "total_enrollment"))
})

test_that("state has 13 subgroups at TOTAL and 14 individual grades", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_total_subgroups <- enr_tidy %>%
    filter(is_state, grade_level == "TOTAL") %>%
    nrow()
  expect_equal(state_total_subgroups, 13)

  state_grades <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level != "TOTAL") %>%
    nrow()
  expect_equal(state_grades, 14)  # PK + K + 01-12
})

test_that("districts have 11 subgroups (no lep/special_ed)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Greenville as example
  gv_subgroups <- enr_tidy %>%
    filter(is_district, district_id == "2301", grade_level == "TOTAL") %>%
    pull(subgroup) %>%
    sort()
  expect_length(gv_subgroups, 11)
  expect_false("lep" %in% gv_subgroups)
  expect_false("special_ed" %in% gv_subgroups)
})

test_that("campuses have 11 subgroups (no lep/special_ed)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Wando High as example
  wando_subgroups <- enr_tidy %>%
    filter(is_campus, campus_id == "1001014", grade_level == "TOTAL") %>%
    pull(subgroup) %>%
    sort()
  expect_length(wando_subgroups, 11)
  expect_false("lep" %in% wando_subgroups)
  expect_false("special_ed" %in% wando_subgroups)
})


# ==============================================================================
# fetch_enr_multi() — multi-year tests
# ==============================================================================

test_that("fetch_enr_multi returns data for multiple years", {
  skip_on_cran()
  skip_if_offline()

  enr_multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(is.data.frame(enr_multi))
  expect_true(2023 %in% enr_multi$end_year)
  expect_true(2024 %in% enr_multi$end_year)
  expect_equal(length(unique(enr_multi$end_year)), 2)
})

test_that("fetch_enr_multi pinned state totals (2023-2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  state_totals <- enr_multi %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    arrange(end_year)

  expect_equal(nrow(state_totals), 2)
  expect_equal(state_totals$n_students[state_totals$end_year == 2023], 789231)
  expect_equal(state_totals$n_students[state_totals$end_year == 2024], 793551)
})

test_that("fetch_enr_multi row count is sum of individual years", {
  skip_on_cran()
  skip_if_offline()

  enr_multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  enr_2023 <- tryCatch(
    fetch_enr(2023, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  enr_2024 <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_equal(nrow(enr_multi), nrow(enr_2023) + nrow(enr_2024))
})

test_that("fetch_enr_multi works with single year", {
  skip_on_cran()
  skip_if_offline()

  enr_single <- tryCatch(
    fetch_enr_multi(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  enr_direct <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_equal(nrow(enr_single), nrow(enr_direct))
})

test_that("fetch_enr_multi wide format works", {
  skip_on_cran()
  skip_if_offline()

  enr_wide_multi <- tryCatch(
    fetch_enr_multi(2023:2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(is.data.frame(enr_wide_multi))
  expect_true(2023 %in% enr_wide_multi$end_year)
  expect_true(2024 %in% enr_wide_multi$end_year)
  expect_true("row_total" %in% names(enr_wide_multi))
})


# ==============================================================================
# enr_grade_aggs() — grade aggregate tests
# ==============================================================================

test_that("enr_grade_aggs produces K8, HS, K12", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  aggs <- enr_grade_aggs(enr_tidy)

  expect_true(is.data.frame(aggs))
  expect_equal(sort(unique(aggs$grade_level)), sort(c("K8", "HS", "K12")))
})

test_that("enr_grade_aggs pinned state values (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  aggs <- enr_grade_aggs(enr_tidy)
  state_aggs <- aggs %>% filter(is_state)

  k8 <- state_aggs$n_students[state_aggs$grade_level == "K8"]
  hs <- state_aggs$n_students[state_aggs$grade_level == "HS"]
  k12 <- state_aggs$n_students[state_aggs$grade_level == "K12"]

  expect_equal(k8, 522034)
  expect_equal(hs, 244054)
  expect_equal(k12, 766088)
})

test_that("enr_grade_aggs K8 + HS = K12 at state level", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  aggs <- enr_grade_aggs(enr_tidy)
  state_aggs <- aggs %>% filter(is_state)

  k8 <- state_aggs$n_students[state_aggs$grade_level == "K8"]
  hs <- state_aggs$n_students[state_aggs$grade_level == "HS"]
  k12 <- state_aggs$n_students[state_aggs$grade_level == "K12"]

  expect_equal(k8 + hs, k12)
})

test_that("enr_grade_aggs K12 excludes PK", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  aggs <- enr_grade_aggs(enr_tidy)
  state_k12 <- aggs %>% filter(is_state, grade_level == "K12") %>% pull(n_students)

  state_total <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  state_pk <- enr_tidy %>%
    filter(is_state, subgroup == "total_enrollment", grade_level == "PK") %>%
    pull(n_students)

  # K12 = total - PK
  expect_equal(state_k12, state_total - state_pk)
})

test_that("enr_grade_aggs has rows for all entity types", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  aggs <- enr_grade_aggs(enr_tidy)

  expect_true(any(aggs$is_state))
  expect_true(any(aggs$is_district))
  expect_true(any(aggs$is_campus))
})

test_that("enr_grade_aggs K8 + HS = K12 for Greenville (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  aggs <- enr_grade_aggs(enr_tidy)
  gv_aggs <- aggs %>% filter(is_district, district_id == "2301")

  k8 <- gv_aggs$n_students[gv_aggs$grade_level == "K8"]
  hs <- gv_aggs$n_students[gv_aggs$grade_level == "HS"]
  k12 <- gv_aggs$n_students[gv_aggs$grade_level == "K12"]

  expect_equal(k8 + hs, k12)
})

test_that("enr_grade_aggs structure", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  aggs <- enr_grade_aggs(enr_tidy)

  # Should have 3 grade levels x (1 state + 82 districts + 1220 campuses) = 3909
  expect_equal(nrow(aggs), 3909)

  # pct should be NA for aggregates
  expect_true(all(is.na(aggs$pct)))

  # subgroup should be total_enrollment
  expect_true(all(aggs$subgroup == "total_enrollment"))
})


# ==============================================================================
# id_enr_aggs() — aggregation flag tests
# ==============================================================================

test_that("id_enr_aggs flags are mutually exclusive", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  # Each row should have exactly one TRUE among is_state, is_district, is_campus
  type_sums <- enr_tidy$is_state + enr_tidy$is_district + enr_tidy$is_campus
  expect_true(all(type_sums == 1))
})

test_that("id_enr_aggs is_charter is always FALSE (no charter_flag data)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(all(enr_tidy$is_charter == FALSE))
})

test_that("id_enr_aggs aggregation_flag matches type column", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  expect_true(all(enr_tidy$aggregation_flag[enr_tidy$is_state] == "state"))
  expect_true(all(enr_tidy$aggregation_flag[enr_tidy$is_district] == "district"))
  expect_true(all(enr_tidy$aggregation_flag[enr_tidy$is_campus] == "campus"))
})


# ==============================================================================
# tidy_enr() direct tests
# ==============================================================================

test_that("tidy_enr produces correct output from wide", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  tidy <- tidy_enr(enr_wide)

  expect_true(is.data.frame(tidy))
  expect_true("grade_level" %in% names(tidy))
  expect_true("subgroup" %in% names(tidy))
  expect_true("n_students" %in% names(tidy))
  expect_true("pct" %in% names(tidy))
})

test_that("tidy_enr removes NA n_students rows", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  tidy <- tidy_enr(enr_wide)

  # All n_students should be non-NA
  expect_true(all(!is.na(tidy$n_students)))
})

test_that("tidy_enr preserves enrollment counts from wide format", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  tidy <- tidy_enr(enr_wide)

  # State total should match
  state_wide_total <- enr_wide$row_total[enr_wide$type == "State"]
  state_tidy_total <- tidy %>%
    filter(type == "State", subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(state_tidy_total, state_wide_total)

  # Greenville total should match
  gv_wide <- enr_wide$row_total[enr_wide$district_id == "2301" & enr_wide$type == "District"]
  gv_tidy <- tidy %>%
    filter(type == "District", district_id == "2301",
           subgroup == "total_enrollment", grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(gv_tidy, gv_wide)
})

test_that("tidy_enr preserves demographic counts from wide format", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  tidy <- tidy_enr(enr_wide)

  # State white count should match wide
  state_wide_white <- enr_wide$white[enr_wide$type == "State"]
  state_tidy_white <- tidy %>%
    filter(type == "State", subgroup == "white", grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(state_tidy_white, state_wide_white)

  # State black count should match wide
  state_wide_black <- enr_wide$black[enr_wide$type == "State"]
  state_tidy_black <- tidy %>%
    filter(type == "State", subgroup == "black", grade_level == "TOTAL") %>%
    pull(n_students)

  expect_equal(state_tidy_black, state_wide_black)
})


# ==============================================================================
# fetch_directory() — directory data tests
# ==============================================================================

test_that("fetch_directory validates year parameter", {
  expect_error(fetch_directory(2017, use_cache = TRUE), "between 2018 and 2025")
  expect_error(fetch_directory(2026, use_cache = TRUE), "between 2018 and 2025")
  expect_error(fetch_directory(2010, use_cache = TRUE), "between 2018 and 2025")
})

test_that("fetch_directory defaults to 2025", {
  skip_on_cran()
  skip_if_offline()

  dir_default <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  expect_equal(unique(dir_default$end_year), 2025L)
})

test_that("fetch_directory tidy has correct columns", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, tidy = TRUE, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  expected_cols <- c(
    "end_year", "state_school_id", "state_district_id",
    "district_name", "school_name", "entity_type", "school_type",
    "address", "city", "state", "zip", "phone", "grades_served",
    "county_name", "superintendent_name", "superintendent_email",
    "principal_name", "principal_email", "website",
    "enrollment", "teacher_count", "board_chair"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(dir_data), info = paste("Missing column:", col))
  }
  expect_equal(ncol(dir_data), 22)
})

test_that("fetch_directory has correct entity types", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  entity_types <- sort(unique(dir_data$entity_type))
  expect_equal(entity_types, sort(c("District", "School")))
})

test_that("fetch_directory pinned row count (2025)", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  expect_equal(nrow(dir_data), 1435)
  expect_equal(sum(dir_data$entity_type == "District"), 81)
  expect_equal(sum(dir_data$entity_type == "School"), 1354)
})

test_that("fetch_directory pinned school types (2025)", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  type_counts <- table(dir_data$school_type)
  expect_equal(as.integer(type_counts["D"]), 81)
  expect_equal(as.integer(type_counts["E"]), 685)
  expect_equal(as.integer(type_counts["H"]), 263)
  expect_equal(as.integer(type_counts["M"]), 360)
  expect_equal(as.integer(type_counts["P"]), 45)
  expect_equal(as.integer(type_counts["S"]), 1)
})

test_that("fetch_directory pinned Greenville district (2025)", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  gv <- dir_data %>%
    filter(grepl("Greenville", district_name), entity_type == "District")
  expect_equal(nrow(gv), 1)
  expect_equal(gv$district_name, "Greenville County School District")
  expect_equal(gv$superintendent_name, "Dr. W. Burke Royster")
  expect_equal(gv$enrollment, 77774)
  expect_equal(gv$state_district_id, "230")
})

test_that("fetch_directory pinned Abbeville district (2025)", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  abb <- dir_data %>%
    filter(grepl("Abbeville", district_name), entity_type == "District")
  expect_equal(nrow(abb), 1)
  expect_equal(abb$state_district_id, "016")
  expect_equal(abb$superintendent_name, "Lori Brewton")
  expect_equal(abb$enrollment, 2746)
})

test_that("fetch_directory state column is always SC", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  non_na_state <- dir_data$state[!is.na(dir_data$state)]
  expect_true(all(non_na_state == "SC"))
})

test_that("fetch_directory county_name is all NA", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  expect_true(all(is.na(dir_data$county_name)))
})

test_that("fetch_directory districts have superintendent names", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  districts <- dir_data %>% filter(entity_type == "District")
  non_na_supers <- sum(!is.na(districts$superintendent_name))
  expect_true(non_na_supers > 50)  # Most districts should have superintendent
})

test_that("fetch_directory schools have principal names", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(2025, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  schools <- dir_data %>% filter(entity_type == "School")
  non_na_principals <- sum(!is.na(schools$principal_name))
  expect_true(non_na_principals > 1000)  # Most schools should have principal
})

test_that("fetch_directory raw format has different columns", {
  skip_on_cran()
  skip_if_offline()

  dir_raw <- tryCatch(
    fetch_directory(2025, tidy = FALSE, use_cache = TRUE),
    error = function(e) {
      if (grepl("Failed to download|HTTP error|SSL|timeout|connection", e$message, ignore.case = TRUE)) {
        skip(paste("screportcards.com unavailable:", e$message))
      }
      stop(e)
    }
  )

  expect_s3_class(dir_raw, "tbl_df")
  expect_gt(nrow(dir_raw), 100)
  # Raw format should NOT have our standardized column names
  expect_false("state_school_id" %in% names(dir_raw))
})


# ==============================================================================
# clear_directory_cache() tests
# ==============================================================================

test_that("clear_directory_cache runs without error", {
  expect_no_error(clear_directory_cache())
})


# ==============================================================================
# No Inf/NaN in any numeric column
# ==============================================================================

test_that("no Inf or NaN in tidy enrollment data (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  for (col in names(enr_tidy)) {
    if (is.numeric(enr_tidy[[col]])) {
      expect_false(
        any(is.infinite(enr_tidy[[col]]), na.rm = TRUE),
        info = paste("Inf found in", col)
      )
      expect_false(
        any(is.nan(enr_tidy[[col]]), na.rm = TRUE),
        info = paste("NaN found in", col)
      )
    }
  }
})

test_that("no Inf or NaN in wide enrollment data (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  for (col in names(enr_wide)) {
    if (is.numeric(enr_wide[[col]])) {
      expect_false(
        any(is.infinite(enr_wide[[col]]), na.rm = TRUE),
        info = paste("Inf found in", col)
      )
      expect_false(
        any(is.nan(enr_wide[[col]]), na.rm = TRUE),
        info = paste("NaN found in", col)
      )
    }
  }
})

test_that("all enrollment counts are non-negative (2024)", {
  skip_on_cran()
  skip_if_offline()

  enr_tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = skip_if_sc_doe_unavailable
  )

  non_na_students <- enr_tidy$n_students[!is.na(enr_tidy$n_students)]
  expect_true(all(non_na_students >= 0))
})
