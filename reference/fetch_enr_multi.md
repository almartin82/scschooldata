# Fetch enrollment data for multiple years

Downloads and combines enrollment data for multiple school years.

## Usage

``` r
fetch_enr_multi(end_years, tidy = TRUE, use_cache = TRUE, count_day = "45")
```

## Arguments

- end_years:

  Vector of school year ends (e.g., c(2022, 2023, 2024))

- tidy:

  If TRUE (default), returns data in long (tidy) format.

- use_cache:

  If TRUE (default), uses locally cached data when available.

- count_day:

  Which count day to use: "45" (default), "135", or "180".

## Value

Combined data frame with enrollment data for all requested years

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 3 years of data
enr_multi <- fetch_enr_multi(2022:2024)

# Track enrollment trends
enr_multi %>%
  dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  dplyr::select(end_year, n_students)
} # }
```
