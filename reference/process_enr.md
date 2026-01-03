# Process raw SCDE enrollment data

Transforms raw Active Student Headcount data into a standardized schema
combining school and district data.

## Usage

``` r
process_enr(raw_data, end_year)
```

## Arguments

- raw_data:

  List containing school_grade, school_demo, district_grade,
  district_demo data frames from get_raw_enr

- end_year:

  School year end

## Value

Processed data frame with standardized columns
