# Prepare bias and indirectness data for adjustment

Aggregates domain-level prior data and calculates total adjustment
values per study result. Supports additive and proportional adjustments.

## Usage

``` r
tri_prep_data(dat_bias, dat_ind)
```

## Arguments

- dat_bias:

  Data with bias priors (long format)

- dat_ind:

  Data with indirectness priors (long format)

## Value

Data with yi/vi and adjustment components ready for adjustment
