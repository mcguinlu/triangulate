# Creating triangulation datasets

**WARNING:** This page is still under construction

``` r
library(triangulate)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## Introduction

This vignette demonstrates how to create a triangulation dataset using
the triangulate package. We will manually construct a small example
dataset and walk through the process of preparing it for triangulation,
including appending bias/indirectness values and visualizing bias
directions.

## Step 1: Create the example dataset

We define a small dataset of six studies, each with a different
combination of bias/indirectness type and direction.

``` r
example_data <- tibble(
  result_id = paste0("S", 1:6),
  study = paste("Study", 1:6),
  yi = c(-.2, .25, -.1, -.3, .1, .3),
  vi = round(runif(6, 0.01, 0.05), 3),
  d1j = rep("moderate", 6),
  d1t = c(rep("add", 2), rep("prop", 4)),
  d1d = c("Favours comparator", "Favours experimental",
          "Away from null", "Towards null",
          "Away from null", "Towards null")
)

example_data
#> # A tibble: 6 × 7
#>   result_id study      yi    vi d1j      d1t   d1d                 
#>   <chr>     <chr>   <dbl> <dbl> <chr>    <chr> <chr>               
#> 1 S1        Study 1 -0.2  0.013 moderate add   Favours comparator  
#> 2 S2        Study 2  0.25 0.043 moderate add   Favours experimental
#> 3 S3        Study 3 -0.1  0.034 moderate prop  Away from null      
#> 4 S4        Study 4 -0.3  0.016 moderate prop  Towards null        
#> 5 S5        Study 5  0.1  0.01  moderate prop  Away from null      
#> 6 S6        Study 6  0.3  0.029 moderate prop  Towards null
```

## Step 2: Convert to long format

Triangulation requires domain-level assessments to be in long format. We
use tri_to_long() to convert from wide to long form.

``` r
example_long <- tri_to_long(example_data)
head(example_long)
#> # A tibble: 6 × 8
#>   result_id study      yi    vi domain j        t     d                   
#>   <chr>     <chr>   <dbl> <dbl> <chr>  <chr>    <chr> <chr>               
#> 1 S1        Study 1 -0.2  0.013 d1     moderate add   favours comparator  
#> 2 S2        Study 2  0.25 0.043 d1     moderate add   favours experimental
#> 3 S3        Study 3 -0.1  0.034 d1     moderate prop  away from null      
#> 4 S4        Study 4 -0.3  0.016 d1     moderate prop  towards null        
#> 5 S5        Study 5  0.1  0.01  d1     moderate prop  away from null      
#> 6 S6        Study 6  0.3  0.029 d1     moderate prop  towards null
```

## Step 3: Add absolute direction

Calculate the absolute direction of bias/indirectness based on type,
direction, and position of the estimate relative to the null.

``` r
example_abs <- tri_absolute_direction(example_long)
head(example_abs)
#> # A tibble: 6 × 8
#>   result_id study      yi    vi domain j        t     d    
#>   <chr>     <chr>   <dbl> <dbl> <chr>  <chr>    <chr> <chr>
#> 1 S1        Study 1 -0.2  0.013 d1     moderate add   right
#> 2 S2        Study 2  0.25 0.043 d1     moderate add   left 
#> 3 S3        Study 3 -0.1  0.034 d1     moderate prop  left 
#> 4 S4        Study 4 -0.3  0.016 d1     moderate prop  right
#> 5 S5        Study 5  0.1  0.01  d1     moderate prop  right
#> 6 S6        Study 6  0.3  0.029 d1     moderate prop  left
```

## Step 4: Append bias priors

Next, we append numerical prior values (mean and SD) corresponding to
domain judgments using tri_append_bias().

``` r
example_bias <- tri_append_bias(example_abs, dat_bias_values)
head(example_bias)
#> # A tibble: 6 × 12
#>   result_id study      yi    vi domain j       t     d     bias_m_add bias_v_add
#>   <chr>     <chr>   <dbl> <dbl> <chr>  <chr>   <chr> <chr>      <dbl>      <dbl>
#> 1 S1        Study 1 -0.2  0.013 d1     modera… add   right       0.09       0.05
#> 2 S2        Study 2  0.25 0.043 d1     modera… add   left       -0.09       0.05
#> 3 S3        Study 3 -0.1  0.034 d1     modera… prop  left        0          0   
#> 4 S4        Study 4 -0.3  0.016 d1     modera… prop  right       0          0   
#> 5 S5        Study 5  0.1  0.01  d1     modera… prop  right       0          0   
#> 6 S6        Study 6  0.3  0.029 d1     modera… prop  left        0          0   
#> # ℹ 2 more variables: bias_m_prop <dbl>, bias_v_prop <dbl>
```
