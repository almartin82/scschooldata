# Fetch South Carolina enrollment data

Downloads and processes enrollment data from the SC Department of
Education Active Student Headcounts.

## Usage

``` r
fetch_enr(end_year, tidy = TRUE, use_cache = TRUE, count_day = "45")
```

## Arguments

- end_year:

  A school year end. Year is the end of the academic year - eg 2023-24
  school year is year '2024'. Valid values are 2013-2026.

- tidy:

  If TRUE (default), returns data in long (tidy) format with subgroup
  column. If FALSE, returns wide format.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from SCDE.

- count_day:

  Which count day to use: "45" (default), "135", or "180". The 45-day
  count is typically taken in October/November.

## Value

Data frame with enrollment data. Wide format includes columns for
district_id, campus_id, names, and enrollment counts by
demographic/grade. Tidy format pivots these counts into subgroup and
grade_level columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 2024 enrollment data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# Get wide format
enr_wide <- fetch_enr(2024, tidy = FALSE)

# Force fresh download (ignore cache)
enr_fresh <- fetch_enr(2024, use_cache = FALSE)

# Use 180-day count instead of 45-day
enr_180 <- fetch_enr(2024, count_day = "180")

# Filter to specific district
greenville <- enr_2024 %>%
  dplyr::filter(grepl("Greenville", district_name))
} # }
```
