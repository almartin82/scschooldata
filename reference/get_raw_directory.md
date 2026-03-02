# Get raw school directory data from SCDE

Downloads the raw school directory Excel file from the SC Report Cards
website. The data comes from the "Additional Info" data file, sheet
"1.MainPage".

## Usage

``` r
get_raw_directory(end_year)
```

## Arguments

- end_year:

  School year end (e.g., 2025 for 2024-25)

## Value

Raw data frame from the 1.MainPage sheet
