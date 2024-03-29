#' Title
#'
#' @param dat Data in long format
#' @param types Vector of study types for which the direction should be reversed
#'
#' @export
#'
tri_swap_effect_direction <- function(dat, types = NULL) {
  dat %>%
    # Standardise effect direction
    dplyr::mutate(yi = ifelse(type %in% types, yi * -1, yi)) %>%
    # Change direction of proportional biases
    dplyr::mutate(d = dplyr::case_when(
      type %in% types & .$t == "prop" & d == "right" ~ "left",
      type %in% types & .$t == "prop" & d == "left" ~ "right",
      T ~ d
    ))
}
