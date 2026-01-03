# Get list of available years for SC enrollment data

Returns the range of years available from different SC data sources.

## Usage

``` r
get_available_years(source = "headcounts")
```

## Arguments

- source:

  Which data source: "headcounts" (default) for Active Student
  Headcounts (2013-2025), or "reportcards" for SC Report Cards
  (2018-2025).

## Value

Integer vector of available years (end_year format, e.g., 2024 for
2023-24)

## Examples

``` r
get_available_years()
#>  [1] 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024
get_available_years("headcounts")
#>  [1] 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024
get_available_years("reportcards")
#> [1] 2018 2019 2020 2021 2022 2023 2024
```
