# Fetch South Carolina school directory data

Downloads and processes school directory data from the SC Report Cards
website. This includes school and district contact information,
principal and superintendent names, addresses, phone numbers, grade
spans, and school websites.

## Usage

``` r
fetch_directory(end_year = NULL, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  School year end (e.g., 2025 for 2024-25). Valid: 2018-2025. If NULL
  (default), uses the most recent available year.

- tidy:

  If TRUE (default), returns data in a standardized format with
  consistent column names. If FALSE, returns raw column names from SCDE.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from SCDE.

## Value

A tibble with school directory data. Tidy columns include:

- `end_year`: School year end

- `state_district_id`: District portion of SCHOOLID (first 3 digits)

- `state_school_id`: Full 7-digit SCHOOLID

- `district_name`: District name

- `school_name`: School name

- `entity_type`: Entity type (School or District)

- `school_type`: Report card type (P=Primary, E=Elementary, M=Middle,
  H=High, D=District, S=Special)

- `address`: Street address

- `city`: City

- `state`: State (always "SC")

- `zip`: ZIP code

- `phone`: Phone number

- `grades_served`: Grade range (e.g., "9-12")

- `county_name`: County (NA – not available in this data source)

- `superintendent_name`: District superintendent name (district rows
  only)

- `superintendent_email`: Superintendent email (NA – not available)

- `principal_name`: School principal name (school rows only)

- `principal_email`: Principal email (NA – not available)

- `website`: School or district website URL

- `enrollment`: Enrollment count

- `teacher_count`: Number of teachers

- `board_chair`: School board chair name (district rows only)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get latest directory data
dir_data <- fetch_directory()

# Get specific year
dir_2024 <- fetch_directory(2024)

# Get raw format (original SCDE column names)
dir_raw <- fetch_directory(tidy = FALSE)

# Force fresh download (ignore cache)
dir_fresh <- fetch_directory(use_cache = FALSE)

# Filter to specific district
library(dplyr)
greenville <- dir_data |>
  filter(grepl("Greenville", district_name))
} # }
```
