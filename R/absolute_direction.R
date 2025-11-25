#' Convert to absolute direction of bias or indirectness
#'
#' Converts qualitative bias direction labels (e.g. "Favours comparator")
#' into absolute directions ("left" or "right") based on effect size and bias type.
#'
#' @param dat Data in long format
#'
#' @export
tri_absolute_direction <- function(dat) {

  # Check for unexpected direction values
  allowed_relative_directions <- c(
    "favours comparator", "favours experimental",
    "towards null", "away from null",
    "unpredictable", "none"
  )
  unexpected_d <- setdiff(tolower(unique(dat$d)), allowed_relative_directions)

  if (length(unexpected_d) > 0) {
    stop(
      "tri_absolute_direction(): Unexpected values in column 'd': ",
      paste(unexpected_d, collapse = ", "),
      ". Please ensure values are properly formatted or run standardisation."
    )
  }

  dat %>%
    # Convert to lower case for matching
    dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%

    # Map to absolute direction
    dplyr::mutate(
      d = dplyr::case_when(
        t == "add" & d == "favours comparator" ~ "right",
        t == "add" & d == "favours experimental" ~ "left",

        t == "prop" & yi < 0 & d == "away from null" ~ "left",
        t == "prop" & yi < 0 & d == "towards null" ~ "right",
        t == "prop" & yi > 0 & d == "away from null" ~ "right",
        t == "prop" & yi > 0 & d == "towards null" ~ "left",

        d == "unpredictable" ~ "unpredictable",
        d == "none" ~ "none",

        TRUE ~ NA_character_ # safer than empty string
      )
    ) %>%
    return()
}

#' Convert absolute direction back to qualitative tool labels
#'
#' Reverses the transformation done by `tri_absolute_direction()`, converting
#' absolute directions ("left"/"right") back to labels like "Favours comparator".
#'
#' @param dat Data in long format
#'
#' @export
tri_absolute_direction_invert <- function(dat) {

  dat %>%
    dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%

    dplyr::mutate(
      d = dplyr::case_when(
        t == "add" & d == "right" ~ "Favours comparator",
        t == "add" & d == "left" ~ "Favours experimental",

        t == "prop" & yi < 0 & d == "left" ~ "Away from null",
        t == "prop" & yi < 0 & d == "right" ~ "Towards null",
        t == "prop" & yi > 0 & d == "right" ~ "Away from null",
        t == "prop" & yi > 0 & d == "left" ~ "Towards null",

        d == "unpredictable" ~ "Unpredictable",
        d == "none" ~ "None",

        TRUE ~ NA_character_
      )
    ) %>%
    # Return direction and others in sentence case
    dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_sentence)) %>%
    return()
}

#' Quick wrapper to apply absolute direction transformation
#'
#' Converts data between wide and long format while applying or reversing
#' the absolute direction transformation.
#'
#' @param dat Dataset in wide format ## check this is the correct parameter
#' @param invert If TRUE, reverse transformation (absolute → qualitative)
#'
#' @export
tri_absolute_direction_quick <- function(dat, invert = FALSE) {

  data <- dat %>%
    tri_to_long() %>%
    {
      if (invert) tri_absolute_direction_invert(.) else tri_absolute_direction(.)
    } %>%
    tri_to_wide()

  return(data)
}



