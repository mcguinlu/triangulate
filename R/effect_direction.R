#' Standardise Effect Direction and Adjust Bias Directions
#'
#' Flips effect estimates (`yi`) and proportional bias directions (`d`)
#' for specified study types (e.g., odds ratios) to ensure consistency
#' across studies using different metrics.
#'
#' @param dat A long-format triangulation dataset
#' @param types A character vector of `type` values to flip (e.g. c("OR", "RR"))
#'
#' @return A modified dataset with adjusted `yi` and `d` values
#' @export
tri_swap_effect_direction <- function(dat, types = NULL) {
  # Defensive checks
  required <- c("type", "yi", "t", "d")
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0) {
    stop("tri_swap_effect_direction(): Missing required columns: ", paste(missing, collapse = ", "))
  }

  dat %>%
    dplyr::mutate(
      yi = ifelse(type %in% types, yi * -1, yi),
      d = dplyr::case_when(
        type %in% types & t == "prop" & d == "right" ~ "left",
        type %in% types & t == "prop" & d == "left" ~ "right",
        TRUE ~ d
      )
    )
}
