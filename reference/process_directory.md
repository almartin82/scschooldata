# Process raw SC directory data to standard schema

Takes raw directory data from the SC Report Cards 1.MainPage sheet and
standardizes column names and types.

## Usage

``` r
process_directory(raw_data, end_year)
```

## Arguments

- raw_data:

  Raw data frame from get_raw_directory()

- end_year:

  School year end

## Value

Processed tibble with standard schema
