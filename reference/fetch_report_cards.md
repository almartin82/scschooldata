# Fetch enrollment data from SC Report Cards

Downloads enrollment data from the SC Report Cards website. This source
provides basic enrollment counts and school information but less
demographic detail than Active Student Headcounts.

## Usage

``` r
fetch_report_cards(end_year, use_cache = TRUE)
```

## Arguments

- end_year:

  School year end (e.g., 2024 for 2023-24). Valid: 2018-2025.

- use_cache:

  If TRUE (default), uses locally cached data when available.

## Value

Data frame with school enrollment and basic information

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Report Cards data
rc_2024 <- fetch_report_cards(2024)

# View school types
table(rc_2024$school_type)
} # }
```
