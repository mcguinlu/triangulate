# Convert data from long to wide format

Reshapes domain-level bias columns from long format to wide format
(e.g., d1j, d1t, d1d). Assumes long format with columns 'domain', 'j',
't', 'd'.

## Usage

``` r
tri_to_wide(dat)
```

## Arguments

- dat:

  A long-format triangulate dataset

## Value

A wide-format data frame
