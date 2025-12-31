# scschooldata

An R package for fetching, processing, and analyzing school enrollment data from the South Carolina Department of Education (SCDE).

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("almartin82/scschooldata")
```

## Quick Start

```r
library(scschooldata)

# Get 2024 enrollment data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# Get wide format (one row per school/district)
enr_wide <- fetch_enr(2024, tidy = FALSE)

# Get multiple years
enr_multi <- fetch_enr_multi(2022:2024)

# Filter to specific district
greenville <- enr_2024 %>%
  dplyr::filter(grepl("Greenville", district_name))
```

## Data Availability

### Primary Data Source: Active Student Headcounts

The package uses SCDE's Active Student Headcounts as the primary data source. This provides the most detailed enrollment breakdowns.

| Years | Availability | Demographics | Grade Levels |
|-------|-------------|--------------|--------------|
| 2013-2026 | Full | Race/ethnicity, gender, poverty status | PK-12 |
| 2012-13 | Partial | Some files available | PK-12 |

**Count Days Available:**
- 45-day count (October/November) - Most commonly used
- 135-day count (March)
- 180-day count (End of year)

### Secondary Data Source: SC Report Cards

SC Report Cards data provides additional school information but less demographic detail.

| Years | Availability | Notes |
|-------|-------------|-------|
| 2018-2025 | Full | Includes school type, teacher count, grade span |
| Pre-2018 | Not available | Data format changed |

### Aggregation Levels

| Level | Available Years | Notes |
|-------|----------------|-------|
| State | 2013-2026 | Calculated from district totals |
| District | 2013-2026 | ~80 school districts |
| School/Campus | 2013-2026 | ~1,200 schools |

### Demographic Categories

| Category | Available Years | Notes |
|----------|----------------|-------|
| Total Enrollment | 2013-2026 | |
| White | 2013-2026 | |
| Black/African American | 2013-2026 | |
| Hispanic/Latino | 2013-2026 | |
| Asian | 2013-2026 | |
| American Indian/Alaska Native | 2013-2026 | |
| Native Hawaiian/Pacific Islander | 2013-2026 | |
| Two or More Races | 2013-2026 | |
| Male/Female | 2013-2026 | |
| Economically Disadvantaged | 2013-2026 | Pupils in Poverty |
| LEP/ELL | Not available | Not in headcount files |
| Special Education | Not available | Not in headcount files |

### Grade Levels

| Grades | Available Years | Notes |
|--------|----------------|-------|
| PK (Pre-K) | 2013-2026 | |
| K (Kindergarten) | 2013-2026 | Listed as K5 in source |
| Grades 1-12 | 2013-2026 | |

### Known Caveats

1. **School IDs**: SC uses 7-digit school codes. The first 3 digits identify the district.

2. **District Names**: District names in the source files contain line breaks that are cleaned during processing.

3. **Charter Schools**: The SC Public Charter School District (code 900) contains state-authorized charter schools. Locally-authorized charters are included in their sponsoring district.

4. **Missing Demographic Data**: LEP and Special Education counts are not included in the Active Student Headcounts. Use SC Report Cards for additional demographic information.

5. **Historical URL Patterns**: SCDE has changed URL patterns several times. The package attempts multiple URL patterns for older years.

## Data Output

### Wide Format (`tidy = FALSE`)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end (2024 = 2023-24) |
| type | character | "State", "District", or "Campus" |
| district_id | character | 3-digit district code |
| campus_id | character | 7-digit school code |
| district_name | character | District name |
| campus_name | character | School name |
| row_total | integer | Total enrollment |
| white, black, hispanic, asian, native_american, pacific_islander, multiracial | integer | Race/ethnicity counts |
| male, female | integer | Gender counts |
| econ_disadv | integer | Economically disadvantaged count |
| grade_pk, grade_k, grade_01-grade_12 | integer | Grade-level enrollment |

### Tidy Format (`tidy = TRUE`, default)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end |
| district_id | character | District code |
| campus_id | character | School code |
| district_name | character | District name |
| campus_name | character | School name |
| type | character | Aggregation level |
| grade_level | character | "TOTAL", "PK", "K", "01"-"12" |
| subgroup | character | "total_enrollment", "white", "black", etc. |
| n_students | integer | Student count |
| pct | numeric | Percentage of total (0-1 scale) |
| is_state | logical | State-level row |
| is_district | logical | District-level row |
| is_campus | logical | School-level row |
| is_charter | logical | Charter school indicator |

## Caching

Downloaded data is cached locally to avoid repeated downloads:

```r
# View cached files
cache_status()

# Clear all cached data
clear_cache()

# Clear specific year
clear_cache(2024)

# Force fresh download
enr <- fetch_enr(2024, use_cache = FALSE)
```

Cache location: `rappdirs::user_cache_dir("scschooldata")`

## Data Sources

- **Active Student Headcounts**: https://ed.sc.gov/data/other/student-counts/active-student-headcounts/
- **SC Report Cards**: https://screportcards.com/
- **SCDE Data Portal**: https://ed.sc.gov/data/

## South Carolina Education Context

- **Total Students**: ~750,000 (2023-24)
- **School Districts**: ~80 (including charter district)
- **Schools**: ~1,400
- **State Education Agency**: South Carolina Department of Education (SCDE)

## Related Packages

This package is part of the `*schooldata` family:
- [txschooldata](https://github.com/almartin82/txschooldata) - Texas
- [caschooldata](https://github.com/almartin82/caschooldata) - California
- [nyschooldata](https://github.com/almartin82/nyschooldata) - New York
- [ilschooldata](https://github.com/almartin82/ilschooldata) - Illinois
- [ohschooldata](https://github.com/almartin82/ohschooldata) - Ohio
- [paschooldata](https://github.com/almartin82/paschooldata) - Pennsylvania

## License

MIT
