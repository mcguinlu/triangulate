# Plot bias-adjusted and unadjusted estimates with RoB

Creates a forest plot of both raw and adjusted effect estimates,
alongside domain-level risk of bias annotations for each study.

## Usage

``` r
tri_plot_bias_direction(
  dat,
  dat_adj = NULL,
  title = NULL,
  grouping = "type",
  rma_method = "REML",
  ...
)
```

## Arguments

- dat:

  A dataframe output from
  [`tri_calculate_adjusted_estimates()`](https://mcguinlu.github.io/triangulate/reference/tri_calculate_adjusted_estimates.md),
  with `yi`, `vi`, `yi_adj`, `vi_adj`, and domain columns.

- dat_adj:

  A data frame with adjusted estimates and variances, if different from
  the main input.

- title:

  Optional plot title.

- grouping:

  Column by which to stratify subgroups (default = `"type"`).

- rma_method:

  Meta-analysis method passed to metafor::rma (e.g., "REML", "FE"). We
  use metafor's default which random-effects

- ...:

  Additional arguments passed to
  [`rob_direction()`](https://mcguinlu.github.io/triangulate/reference/rob_direction.md).

## Value

A forest plot is drawn (base graphics).
