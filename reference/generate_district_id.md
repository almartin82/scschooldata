# Generate district ID from district name

Creates a consistent 3-character ID from district name. SC uses numeric
district codes, but since they're not in the headcount files, we
generate a consistent identifier.

## Usage

``` r
generate_district_id(name)
```

## Arguments

- name:

  District name

## Value

3-character district ID
