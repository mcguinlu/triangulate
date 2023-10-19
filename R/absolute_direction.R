#' Title
#'
#' @param dat Data in long format
#'
#' @export
tri_absolute_direction <- function(dat){
  dat %>%
    # Covert to lower case
    dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%
    # Convert from absolute direction back to response to tool
    dplyr::mutate(
      # Additive
      d = dplyr::case_when(
        .$t == "add" & d == "favours comparator" ~ "right",
        .$t == "add" & d == "favours experimental" ~ "left",
        .$t == "prop" & d == "away from null" & yi < 0 ~ "left",
        .$t == "prop" & d == "towards null" & yi > 0 ~ "left",
        .$t == "prop" & d == "towards null" & yi < 0 ~ "right",
        .$t == "prop" & d == "away from null" & yi > 0 ~ "right",
        .$d == "unpredictable" ~ "unpredictable",
        .$d == "none" ~ "none",
        T ~ ""
      )
    ) %>%
    return()
}

#' Title
#'
#' @param dat Data in long format
#'
#' @export
tri_absolute_direction_invert <- function(dat){
  dat %>%
  # Covert to lower case
  dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%
  # Convert from absolute direction back to response to tool
  dplyr::mutate(
    # Additive
    d = dplyr::case_when(
      .$t == "add" & d == "right"  ~ "Favours comparator",
      .$t == "add" & d == "left"  ~ "Favours experimental",
      .$t == "prop" & d == "left" & yi < 0 ~ "Away from null",
      .$t == "prop" & d == "left" & yi > 0 ~ "Towards null",
      .$t == "prop" & d == "right" & yi < 0 ~ "Towards null",
      .$t == "prop" & d == "right" & yi > 0 ~ "Away from null",
      .$d == "unpredictable" ~ "Unpredictable",
      .$d == "none" ~ "None",
      T ~ ""
    )
  ) %>%
  # Convert to sentence case
  dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_sentence)) %>%
  return()
  }


#' Title
#'
#' @description A helper function to quickly convert from relative to absolute
#'   directions of bias, or vice versa
#'
#' @param dat Triangulation dataset
#' @param invert Direction indicator
#'
#' @export
tri_absolute_direction_quick <- function(dat, invert = FALSE){

  if (invert) {

    dat %>%
      tri_to_long() %>%
      tri_absolute_direction_invert() %>%
      tri_to_wide() %>%
      return()

  } else {

    dat %>%
      tri_to_long() %>%
      tri_absolute_direction() %>%
      tri_to_wide() %>%
      return()

  }

}


