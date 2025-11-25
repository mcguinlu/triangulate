# Bias direction plots

Bias direction plots

## Usage

``` r
rob_direction(
  dat,
  sei = NULL,
  title = NULL,
  legend_cex = 0.9,
  grouping = "type",
  grouping_levels = c("MR", "NRSI", "Obs", "RCT"),
  label_subgroup_summary = "RE Model for Subgroup",
  rma_method = "REML",
  ...
)
```

## Arguments

- dat:

  Dataframe

- sei:

  Vector containing the corresponding standard errors (normally defined
  as the column within the dataset, i.e. dat\$sei). Note: either vi or
  sei must be set.

- title:

  Graph title

- legend_cex:

  Expansion factor for figure legend.

- grouping:

  Variable of the provided dataset by which the resulting plot will be
  stratified. Often will study design or overall risk-of-bias level.

- grouping_levels:

  Ordering of grouping variable. Note: the levels will be plotted in
  order, starting at the bottom of the graph (i.e. the last item in the
  vector will be placed at the top of the graph)

- label_subgroup_summary:

  Annotation text for subgroup label

- ...:

  Other arguments to pass to metafor::forest
