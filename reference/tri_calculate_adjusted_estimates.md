# Calculate Bias-Adjusted Estimates

Computes adjusted effect estimates (`yi_adj`) and variances (`vi_adj`)
incorporating additive and proportional bias adjustments.

## Usage

``` r
tri_calculate_adjusted_estimates(dat)
```

## Arguments

- dat:

  A dataset prepared using
  [`tri_prep_data()`](https://mcguinlu.github.io/triangulate/reference/tri_prep_data.md),
  containing bias parameters

## Value

A tibble with added `yi_adj` and `vi_adj` columns
