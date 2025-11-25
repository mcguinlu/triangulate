# Adding additional levels of bias and indirectness

Say we wish to include ‘Very high’ or ‘Critical’ risk of bias as an
option in our assessments. In this example, Domain 1 of Study 1 has a
“Critical” risk of bias assessment

``` r
dat_bias_with_critical <- triangulate::dat_bias
dat_bias_with_critical[1,6] <- 'Critical'

dat_bias_with_critical[1,1:6]
#> # A tibble: 1 × 6
#>   result_id study   type      yi    vi d1j     
#>   <chr>     <chr>   <chr>  <dbl> <dbl> <chr>   
#> 1 13401-2   Study 1 NRSI  -0.128 0.125 Critical
```

This requires us to add in another prior (‘critical’) to the library
contained in `dat_bias_values`:

``` r
custom_bias_priors <- triangulate::dat_bias_values %>% 
  add_row(domain = 'all',
          j = 'critical',
          bias_m_add = 0.2,
          bias_v_add = 0.12,
          bias_m_prop = 0.08,
          bias_v_prop = 0.04)

custom_bias_priors
#>   domain        j bias_m_add bias_v_add bias_m_prop bias_v_prop
#> 1    all     high       0.18       0.10        0.06       0.032
#> 2    all moderate       0.09       0.05        0.03       0.016
#> 3    all      low       0.00       0.00        0.00       0.000
#> 4    all critical       0.20       0.12        0.08       0.040
```

It is then just a case of passing our custom prior dataset to the prep
functions:

``` r

dat_bias_with_critical_prepped <- dat_bias_with_critical %>%
  
  tri_to_long() %>%
  
  tri_absolute_direction() %>%
  
  tri_append_bias(custom_bias_priors)

dat_bias_with_critical_prepped[1:7,c(2,6:13)]
#> # A tibble: 7 × 9
#>   study   domain j     t     d     bias_m_add bias_v_add bias_m_prop bias_v_prop
#>   <chr>   <chr>  <chr> <chr> <chr>      <dbl>      <dbl>       <dbl>       <dbl>
#> 1 Study 1 d1     crit… add   right        0.2       0.12        0          0    
#> 2 Study 1 d2     low   none  none         0         0           0          0    
#> 3 Study 1 d3     mode… prop  unpr…        0         0           0          0.016
#> 4 Study 1 d4     low   none  none         0         0           0          0    
#> 5 Study 1 d5     low   none  none         0         0           0          0    
#> 6 Study 1 d6     low   none  none         0         0           0          0    
#> 7 Study 1 d7     mode… prop  left         0         0          -0.03       0.016
```

We can see now that the first domain (D1) of Study 1 has been defined as
Critical, and the adjustment values we added to our `custom_bias_priors`
dataset have been correctly assigned to it.

The same approach is possible if you wish to change/supplement the
levels used in the indirectness adjustment.
