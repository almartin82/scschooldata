# Download raw enrollment data from SCDE

Downloads school and district enrollment data from SC Department of
Education. Uses Active Student Headcounts for grade and demographic
breakdowns.

## Usage

``` r
get_raw_enr(end_year, count_day = "45")
```

## Arguments

- end_year:

  School year end (2023-24 = 2024)

- count_day:

  Which count day to use: "45", "135", or "180" (default: "45")

## Value

List with school_grade, school_demo, district_grade, district_demo data
frames
