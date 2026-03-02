# Build candidate URLs for SC Report Cards additional info download

SC DOE has changed URL patterns multiple times. This function generates
all known patterns for a given year so the download function can try
them.

## Usage

``` r
build_directory_urls(end_year)
```

## Arguments

- end_year:

  School year end (e.g., 2025 for 2024-25)

## Value

Vector of candidate URLs to try
