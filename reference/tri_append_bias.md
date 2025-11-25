# Append bias priors to bias dataset

Joins the appropriate prior parameters to a long-format bias dataset,
based on the risk-of-bias judgment (j), direction (d), and type (t).

## Usage

``` r
tri_append_bias(dat, values = NULL, common = TRUE)
```

## Arguments

- dat:

  A bias dataset in long format (must include j, d, t, domain,
  result_id)

- values:

  A data frame of prior distributions (e.g., from dat_bias_values)

- common:

  Should a single set of prior distributions be used across all domains
  (default is TRUE)? Set to FALSE to allow domain-specific priors.

## Value

Dataset with numeric bias prior values appended
