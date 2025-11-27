# Interactive Sensitivity Analysis

## Introduction

This vignette demonstrates how to use the
[`interactive_bias_plot()`](https://mcguinlu.github.io/triangulate/reference/interactive_bias_plot.md)
function to explore how bias priors affect triangulated effect estimates
in real time using a Shiny interface.

The function launches an interactive app with sliders that control the
magnitude and uncertainty of additive and proportional bias. The
adjusted effect estimates update automatically as the priors change.

## Example Data

We use a small example dataset with four hypothetical studies.

``` r
example_data <- tibble::tibble(
  result_id = paste0("S", 1:4),
  study = paste("Study", 1:4),
  yi = c(-0.2, 0.25, -0.1, 0.15),
  vi = c(0.02, 0.03, 0.015, 0.025),
  type = "RCT"
)
```

## Launch Interactive App

To run the app, call the function below. This will open a Shiny window
(locally) allowing you to interactively adjust the priors.

``` r
interactive_bias_plot(example_data)
```

> 📌 **Note**: This command must be run in an interactive R session
> (e.g., RStudio). It won’t execute inside a knitted document but will
> launch a Shiny app when run line-by-line.

## Conclusion

This tool is useful for exploring how robust your triangulated estimates
are to different prior assumptions. You can use it with your own data by
passing a data frame with columns:

- `result_id`
- `study`
- `type`
- `yi` (effect estimate)
- `vi` (variance)
