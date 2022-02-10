#' Title
#'
#' @param dat
#'
#' @return
#' @export
tri_absolute_direction <- function(dat){
  dat %>%
    # Covert to lower case
    dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%
    # Convert from absolute direction back to response to tool
    dplyr::mutate(
      # Additive
      d = dplyr::case_when(
        .$t == "add" & d == "favours comparator" ~ "left",
        .$t == "add" & d == "favours experimental" ~ "right",
        .$t == "add" & d == "favors comparator" ~ "left",
        .$t == "add" & d == "favors experimental" ~ "right",
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
#' @param dat
#'
#' @return
#' @export
tri_absolute_direction_invert <- function(dat){
  dat %>%
  # Covert to lower case
  dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%
  # Convert from absolute direction back to response to tool
  dplyr::mutate(
    # Additive
    d = dplyr::case_when(
      .$t == "add" & d == "left"  ~ "Favours comparator",
      .$t == "add" & d == "right"  ~ "Favours experimental",
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


