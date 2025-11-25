# Convert data from wide to long format

Reshapes domain-level bias columns from wide format (e.g., d1j, d1t,
d1d) to long format. Assumes columns follow naming pattern like d1j,
d1t, d1d, d2j, etc.

## Usage

``` r
tri_to_long(dat)
```

## Arguments

- dat:

  A data frame in wide format

## Value

A long-format data frame with columns: domain, j, t, d
