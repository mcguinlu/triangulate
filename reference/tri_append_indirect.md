# Append indirectness priors to dataset

Similar to tri_append_bias(), but operates on indirectness judgments and
priors

## Usage

``` r
tri_append_indirect(dat, values, common = TRUE)
```

## Arguments

- dat:

  A long-format indirectness dataset

- values:

  A dataframe of indirectness priors (e.g., dat_ind_values)

- common:

  Should a single set of distributions be used across all domains
  (default is TRUE)?

## Value

Dataset with indirectness prior values appended
