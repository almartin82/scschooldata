# Build URL for headcount file

URL patterns have changed over the years. This function attempts to
build the correct URL for a given year.

## Usage

``` r
build_headcount_url(end_year, school_year, level, data_type, count_day)
```

## Arguments

- end_year:

  School year end

- school_year:

  School year string

- level:

  "school" or "district"

- data_type:

  "grade" or "demo"

- count_day:

  "45", "135", or "180"

## Value

URL string
