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
        .$t == "prop" & yi < 0 & d == "away from null" ~ "left",
        .$t == "prop" & yi < 0 & d == "towards null"  ~ "right",
        .$t == "prop" & yi > 0 & d == "away from null"  ~ "right",
        .$t == "prop" & yi > 0 & d == "towards null"  ~ "left",
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
      .$t == "prop" & yi < 0 & d == "left"   ~ "Away from null",
      .$t == "prop" & yi < 0 & d == "right"  ~ "Towards null",
      .$t == "prop" & yi > 0 & d == "right"  ~ "Away from null",
      .$t == "prop" & yi > 0 & d == "left"   ~ "Towards null",
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

    data <- dat %>%
      tri_to_long() %>%
      tri_absolute_direction_invert() %>%
      tri_to_wide()

  } else {

    data <- dat %>%
      tri_to_long() %>%
      tri_absolute_direction() %>%
      tri_to_wide()

  }

  return(data)
}


