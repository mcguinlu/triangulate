# Standardise Effect Direction and Adjust Bias Directions

Flips effect estimates (`yi`) and proportional bias directions (`d`) for
specified study types (e.g., odds ratios) to ensure consistency across
studies using different metrics.

## Usage

``` r
tri_swap_effect_direction(dat, types = NULL)
```

## Arguments

- dat:

  A long-format triangulation dataset

- types:

  A character vector of `type` values to flip (e.g. c("OR", "RR"))

## Value

A modified dataset with adjusted `yi` and `d` values
