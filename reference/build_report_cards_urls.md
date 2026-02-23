# Build candidate URLs for Report Cards data download

SC DOE has changed URL patterns multiple times. This function generates
all known patterns for a given year so the download function can try
them.

## Usage

``` r
build_report_cards_urls(end_year)
```

## Arguments

- end_year:

  School year end (e.g., 2024 for 2023-24)

## Value

Vector of candidate URLs to try
