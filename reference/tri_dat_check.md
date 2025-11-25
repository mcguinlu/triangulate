# Check that required columns are present

Validates whether key variables are present in the dataset. Can toggle
between a basic ("minimal") check or a "full" check including columns
needed for bias adjustment.

## Usage

``` r
tri_dat_check(dat, mode = c("minimal", "full"))
```

## Arguments

- dat:

  A data frame (long or wide format)

- mode:

  Check mode: either "minimal" or "full"

## Value

Throws an error if required columns are missing. Otherwise returns TRUE
(invisibly).
