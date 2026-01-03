# Read headcount Excel file

Parses the SCDE headcount Excel file which has a non-standard header
structure. The file has 5-7 rows of metadata/headers before the data
starts.

## Usage

``` r
read_headcount_excel(file_path, level, data_type)
```

## Arguments

- file_path:

  Path to Excel file

- level:

  "school" or "district"

- data_type:

  "grade" or "demo"

## Value

Data frame with standardized column names
