# Interactive bias-adjusted plot (horizontal layout)

Launches a Shiny app that allows users to adjust additive and
proportional bias priors and visualizes both original and adjusted
estimates horizontally.

## Usage

``` r
interactive_bias_plot(dat)
```

## Arguments

- dat:

  A data.frame with columns: result_id, yi, vi, study, type

## Value

A Shiny app object
