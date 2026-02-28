# scschooldata

**[Documentation](https://almartin82.github.io/scschooldata/)** \|
[GitHub](https://github.com/almartin82/scschooldata)

Fetch and analyze South Carolina school enrollment data from the South
Carolina Department of Education (SCDE) in R or Python. **Over a decade
of data** (2015-2025) for every school, district, and the state.

Part of the [state schooldata
project](https://github.com/almartin82?tab=repositories&q=schooldata),
inspired by [njschooldata](https://github.com/almartin82/njschooldata) -
the original package that started this effort to make state education
data accessible.

## What can you find with scschooldata?

South Carolina enrolls **nearly 800,000 students** across 80 school
districts. There are stories hiding in these numbers. Here are fifteen
insights waiting to be explored:

------------------------------------------------------------------------

### 1. South Carolina is growing

Unlike many states facing enrollment decline, South Carolina has added
approximately 40,000 students since 2015. The Palmetto State’s
population growth is reflected in its schools.

``` r
library(scschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))

enr <- fetch_enr_multi(c(2015, 2017, 2019, 2021, 2023, 2025), use_cache = TRUE)

state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(
    change = n_students - lag(n_students),
    pct_change = round(change / lag(n_students) * 100, 2)
  )
stopifnot(nrow(state_totals) > 0)

state_totals
```

![South Carolina Public School Enrollment
(2015-2025)](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

South Carolina Public School Enrollment (2015-2025)

------------------------------------------------------------------------

### 2. Greenville County is the giant

Greenville County Schools enrolls nearly 77,000 students, making it the
largest district in the state and one of the largest in the Southeast.

``` r
enr_2025 <- fetch_enr(2025, use_cache = TRUE)

top_districts <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)
stopifnot(nrow(top_districts) > 0)

top_districts
```

![Top 10 South Carolina Districts by Enrollment
(2025)](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

Top 10 South Carolina Districts by Enrollment (2025)

------------------------------------------------------------------------

### 3. Hispanic enrollment is surging

Hispanic student enrollment has risen steadily, growing from about 10%
in 2019 to over 14% of total enrollment in 2025.

``` r
demographics <- enr_2025 |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian",
                         "native_american", "pacific_islander", "multiracial")) |>
  mutate(pct = round(n_students / sum(n_students, na.rm = TRUE) * 100, 1)) |>
  select(subgroup, n_students, pct) |>
  arrange(desc(n_students))
stopifnot(nrow(demographics) > 0)

demographics
```

![South Carolina Student Demographics
(2025)](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

South Carolina Student Demographics (2025)

------------------------------------------------------------------------

### 4. The I-85 Corridor is booming

Districts along the I-85 corridor from Greenville through Spartanburg
are among the fastest-growing in the state, fueled by economic
development and migration from other states.

``` r
i85_districts <- enr_2025 |>
  filter(
    grepl("Greenville|Spartanburg|Anderson", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(8)
stopifnot(nrow(i85_districts) > 0)

i85_districts
```

![I-85 Corridor Districts
(2025)](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/regional-chart-1.png)

I-85 Corridor Districts (2025)

------------------------------------------------------------------------

### 5. The Lowcountry is expanding

Charleston, Berkeley, and Dorchester counties form South Carolina’s
tri-county Lowcountry region, and all three have seen substantial
enrollment growth.

``` r
lowcountry_enr <- fetch_enr_multi(c(2015, 2020, 2025), use_cache = TRUE)

lowcountry <- lowcountry_enr |>
  filter(
    grepl("Charleston|Berkeley|Dorchester", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  select(end_year, district_name, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students) |>
  mutate(
    growth = `2025` - `2015`,
    pct_growth = round(growth / `2015` * 100, 1)
  ) |>
  arrange(desc(growth))
stopifnot(nrow(lowcountry) > 0)

lowcountry
```

![Lowcountry Enrollment Growth
(2015-2025)](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/growth-chart-1.png)

Lowcountry Enrollment Growth (2015-2025)

------------------------------------------------------------------------

### 6. South Carolina’s racial composition is shifting fast

In just 8 years, the white share of enrollment has dropped from over 51%
to under 48%, while Hispanic enrollment has surged from 9% to nearly
15%.

``` r
demo_enr <- fetch_enr_multi(c(2017, 2020, 2025), use_cache = TRUE)

demo_shift <- demo_enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "multiracial")) |>
  select(end_year, subgroup, n_students) |>
  group_by(end_year) |>
  mutate(pct = round(n_students / sum(n_students, na.rm = TRUE) * 100, 1)) |>
  ungroup()
stopifnot(nrow(demo_shift) > 0)

demo_shift |>
  select(end_year, subgroup, n_students, pct) |>
  pivot_wider(names_from = end_year, values_from = c(n_students, pct))
```

![South Carolina’s Shifting Demographics
(2017-2025)](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/demo-shift-chart-1.png)

South Carolina’s Shifting Demographics (2017-2025)

------------------------------------------------------------------------

### 7. State-authorized charters are growing fast

The SC Public Charter School District (code 900) serves state-authorized
charter schools and has grown to over 21,000 students.

``` r
charter_enr <- fetch_enr_multi(c(2015, 2020, 2025), use_cache = TRUE)

charter_trends <- charter_enr |>
  filter(
    grepl("Charter School District", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  select(end_year, n_students)
stopifnot(nrow(charter_trends) > 0)

charter_trends

# Charter as percent of state
charter_pct <- enr_2025 |>
  filter(is_state | grepl("Charter School District", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(district_name, n_students) |>
  mutate(type = ifelse(is.na(district_name), "State Total", "Charter")) |>
  select(type, n_students)

charter_pct
```

![Charter District Growth
(2015-2025)](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/charter-chart-1.png)

Charter District Growth (2015-2025)

------------------------------------------------------------------------

### 8. Kindergarten has not recovered from COVID

Kindergarten enrollment dropped sharply during the pandemic and has not
returned to pre-pandemic levels, declining each year since a partial
rebound in 2022.

``` r
k_enr <- fetch_enr_multi(2019:2025, use_cache = TRUE)

k_trends <- k_enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students) |>
  mutate(
    change_from_2019 = n_students - first(n_students),
    pct_change = round(change_from_2019 / first(n_students) * 100, 1)
  )
stopifnot(nrow(k_trends) > 0)

k_trends
```

![Kindergarten Enrollment: Pandemic Impact & Continued
Decline](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/k-chart-1.png)

Kindergarten Enrollment: Pandemic Impact & Continued Decline

------------------------------------------------------------------------

### 9. Rural Pee Dee districts are declining

While much of South Carolina grows, rural districts in the Pee Dee
region face persistent enrollment decline.

``` r
pee_dee_enr <- fetch_enr_multi(c(2015, 2025), use_cache = TRUE)

pee_dee <- pee_dee_enr |>
  filter(
    grepl("Marion|Dillon|Marlboro|Florence", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  select(end_year, district_name, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students) |>
  mutate(
    change = `2025` - `2015`,
    pct_change = round(change / `2015` * 100, 1)
  ) |>
  filter(!is.na(`2015`), !is.na(`2025`)) |>
  arrange(pct_change) |>
  head(8)
stopifnot(nrow(pee_dee) > 0)

pee_dee
```

![Pee Dee Districts: A Decade of
Decline](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/pee-dee-chart-1.png)

Pee Dee Districts: A Decade of Decline

------------------------------------------------------------------------

### 10. District size varies dramatically

South Carolina’s 80 districts range from tiny rural systems to massive
county-wide operations serving tens of thousands.

``` r
district_sizes <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  mutate(size_bucket = case_when(
    n_students < 5000 ~ "Small (<5K)",
    n_students < 15000 ~ "Medium (5K-15K)",
    n_students < 30000 ~ "Large (15K-30K)",
    TRUE ~ "Very Large (30K+)"
  )) |>
  count(size_bucket) |>
  mutate(size_bucket = factor(size_bucket,
                              levels = c("Small (<5K)", "Medium (5K-15K)",
                                         "Large (15K-30K)", "Very Large (30K+)")))
stopifnot(nrow(district_sizes) > 0)

district_sizes
```

![South Carolina District Size
Distribution](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/district-sizes-chart-1.png)

South Carolina District Size Distribution

------------------------------------------------------------------------

### 11. Richland vs Lexington: Columbia’s Suburban Divide

The Columbia metro area is split between Richland and Lexington
counties, with very different enrollment trajectories. Lexington County
districts have grown substantially while Richland districts have been
more stable.

``` r
columbia_enr <- fetch_enr_multi(c(2015, 2020, 2025), use_cache = TRUE)

columbia_districts <- columbia_enr |>
  filter(
    grepl("Richland|Lexington", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  select(end_year, district_name, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students) |>
  mutate(
    growth = `2025` - `2015`,
    pct_growth = round(growth / `2015` * 100, 1),
    county = ifelse(grepl("Richland", district_name), "Richland", "Lexington")
  ) |>
  filter(!is.na(`2015`), !is.na(`2025`)) |>
  arrange(desc(growth))
stopifnot(nrow(columbia_districts) > 0)

columbia_districts
```

![Columbia Metro: Lexington Growth Outpaces
Richland](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/columbia-metro-chart-1.png)

Columbia Metro: Lexington Growth Outpaces Richland

------------------------------------------------------------------------

### 12. Boys slightly outnumber girls statewide

South Carolina enrolls slightly more male than female students overall,
a pattern consistent across most large districts.

``` r
gender_data <- enr_2025 |>
  filter(
    is_district,
    subgroup %in% c("male", "female"),
    grade_level == "TOTAL"
  ) |>
  select(district_name, subgroup, n_students) |>
  pivot_wider(names_from = subgroup, values_from = n_students) |>
  filter(!is.na(male), !is.na(female)) |>
  mutate(
    total = male + female,
    pct_male = round(male / total * 100, 1)
  ) |>
  arrange(desc(total)) |>
  head(15)
stopifnot(nrow(gender_data) > 0)

gender_data
```

![Gender Balance in South Carolina’s Largest
Districts](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/gender-gap-chart-1.png)

Gender Balance in South Carolina’s Largest Districts

------------------------------------------------------------------------

### 13. High School Enrollment: The 9th Grade Bulge

High schools show a distinctive enrollment pattern: 9th grade is
consistently the largest, with enrollment declining through 12th grade.
This reflects retention, transfers, and dropouts.

``` r
hs_grades <- enr_2025 |>
  filter(
    is_state,
    subgroup == "total_enrollment",
    grade_level %in% c("09", "10", "11", "12")
  ) |>
  select(grade_level, n_students) |>
  mutate(
    grade_label = case_when(
      grade_level == "09" ~ "9th Grade",
      grade_level == "10" ~ "10th Grade",
      grade_level == "11" ~ "11th Grade",
      grade_level == "12" ~ "12th Grade"
    ),
    grade_label = factor(grade_label, levels = c("9th Grade", "10th Grade", "11th Grade", "12th Grade")),
    pct_of_9th = round(n_students / first(n_students) * 100, 1)
  )
stopifnot(nrow(hs_grades) > 0)

hs_grades
```

![The 9th Grade
Bulge](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/high-school-chart-1.png)

The 9th Grade Bulge

------------------------------------------------------------------------

### 14. South Carolina’s Smallest Districts

Not all South Carolina districts are massive county systems. Several
rural districts serve fewer than 2,000 students.

``` r
smallest <- enr_2025 |>
  filter(
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL",
    !grepl("Charter", district_name)
  ) |>
  arrange(n_students) |>
  select(district_name, n_students) |>
  head(10)
stopifnot(nrow(smallest) > 0)

smallest
```

![South Carolina’s Smallest
Districts](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/smallest-districts-chart-1.png)

South Carolina’s Smallest Districts

------------------------------------------------------------------------

### 15. Charleston’s Decade of Transformation

Charleston 01 (Charleston County) has undergone significant demographic
change in recent years, reflecting the city’s rapid growth and
gentrification.

``` r
charleston_demo <- fetch_enr_multi(c(2017, 2020, 2025), use_cache = TRUE)

charleston_demo_trends <- charleston_demo |>
  filter(
    grepl("Charleston", district_name),
    is_district,
    grade_level == "TOTAL",
    subgroup %in% c("white", "black", "hispanic", "asian", "multiracial")
  ) |>
  select(end_year, subgroup, n_students) |>
  group_by(end_year) |>
  mutate(pct = round(n_students / sum(n_students, na.rm = TRUE) * 100, 1)) |>
  ungroup()
stopifnot(nrow(charleston_demo_trends) > 0)

charleston_demo_trends |>
  pivot_wider(names_from = end_year, values_from = c(n_students, pct))
```

![Charleston County: A Changing
District](https://almartin82.github.io/scschooldata/articles/enrollment_hooks_files/figure-html/charleston-demographics-chart-1.png)

Charleston County: A Changing District

------------------------------------------------------------------------

## Installation

``` r
# install.packages("devtools")
devtools::install_github("almartin82/scschooldata")
```

## Quick Start

### R

``` r
library(scschooldata)
library(dplyr)

# Get 2025 enrollment data (2024-25 school year)
enr <- fetch_enr(2025, use_cache = TRUE)

# Statewide total
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  pull(n_students)

# Top 10 districts
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(10)

# Get multiple years
enr_multi <- fetch_enr_multi(2020:2025, use_cache = TRUE)
```

### Python

``` python
import pyscschooldata as sc

# Get 2025 enrollment data (2024-25 school year)
df = sc.fetch_enr(2025)

# Statewide total
state_total = df[(df['is_state'] == True) &
                 (df['subgroup'] == 'total_enrollment') &
                 (df['grade_level'] == 'TOTAL')]['n_students'].values[0]
print(f"Total enrollment: {state_total:,}")

# Top 10 districts
districts = df[(df['is_district'] == True) &
               (df['subgroup'] == 'total_enrollment') &
               (df['grade_level'] == 'TOTAL')]
print(districts.nlargest(10, 'n_students')[['district_name', 'n_students']])

# Get multiple years
df_multi = sc.fetch_enr_multi([2020, 2021, 2022, 2023, 2024, 2025])
```

## Data Notes

### Source

Data is sourced directly from the South Carolina Department of Education
[Active Student
Headcounts](https://ed.sc.gov/data/other/student-counts/active-student-headcounts/).

### Available Years

- **2013-2025** (13 years of data)
- Data reflects the 45-day count (early fall enrollment snapshot)

### Suppression Rules

South Carolina does not suppress small cell counts in the Active Student
Headcounts data. All counts are reported as-is from the state.

### Data Quality Notes

- School and district names may vary slightly across years
- Some schools open/close between years
- Charter schools are identified by `is_charter = TRUE`
- State-authorized charters are grouped under District 900 (SC Public
  Charter School District)

### Census Day

The 45-day count typically occurs in late October/early November,
representing enrollment approximately 45 school days into the academic
year.

## Data Availability

| Years     | Source                    | Notes                           |
|-----------|---------------------------|---------------------------------|
| 2013-2025 | Active Student Headcounts | Full demographics, grades PK-12 |

**13 years** across ~80 districts and ~1,400 schools.

### What’s Included

- **Levels:** State, district, and school
- **Demographics:** White, Black, Hispanic, Asian, Native American,
  Pacific Islander, Multiracial
- **Gender:** Male, Female
- **Special populations:** Economically disadvantaged (Pupils in
  Poverty)
- **Grade levels:** Pre-K through Grade 12

### What’s NOT Available

- LEP/ELL counts (not in Active Student Headcounts files)
- Special education counts (separate data system)

### South Carolina ID System

- **District ID:** 3-digit code (first 3 digits of school code)
- **School ID:** 7-digit code
- **Charter District:** Code 900 for state-authorized charters

## Data Format

| Column                         | Description                              |
|--------------------------------|------------------------------------------|
| `end_year`                     | School year end (e.g., 2025 for 2024-25) |
| `district_id`                  | 3-digit district code                    |
| `campus_id`                    | 7-digit school code                      |
| `district_name`, `campus_name` | Names                                    |
| `type`                         | “State”, “District”, or “Campus”         |
| `grade_level`                  | “TOTAL”, “PK”, “K”, “01”…“12”            |
| `subgroup`                     | Demographic group                        |
| `n_students`                   | Enrollment count                         |
| `pct`                          | Percentage of total                      |
| `is_charter`                   | Charter school indicator                 |

## Caching

``` r
# View cached files
cache_status()

# Clear cache
clear_cache()

# Force fresh download
enr <- fetch_enr(2025, use_cache = FALSE)
```

## More Insights

See the [full
vignette](https://almartin82.github.io/scschooldata/articles/enrollment_hooks.html)
for additional analysis and visualizations.

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data
in Python and R.

**All 50 state packages:**
[github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

Inspired by [njschooldata](https://github.com/almartin82/njschooldata) -
making education data accessible one state at a time.

## Author

Andy Martin (<almartin@gmail.com>) GitHub:
[github.com/almartin82](https://github.com/almartin82)

## License

MIT
