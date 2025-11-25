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
tri_swap_effect_direction <- function(dat, types = NULL) { # takes triangulate long format dataset and vector of types of studies you want to flip
  # example input tri_swap_effect_direction(dat, types = c("OR")) would swap effect size and prop bias for rows where type == OR

  # types = NULL is so that it doesnt flip types unless user tells it to
  # Defensive checks - ensures the input data has everything needed for the swap logic
  required <- c("type", "yi", "t", "d") # study type, effect size, add/prop bias, direction (left/right)
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0) {
    stop("tri_swap_effect_direction(): Missing required columns: ", paste(missing, collapse = ", "))
  }

  dat %>%
    dplyr::mutate(
      yi = ifelse(type %in% types, yi * -1, yi), # if a type is on the list of types to flip, you invert the sign of the effect estimate
      # flips the effect estimate
      d = dplyr::case_when(
        type %in% types & t == "prop" & d == "right" ~ "left",
        # only proportional bias flipped because additive is symmetrical but prop is scale-dependent
        # if the sign of yi is flipped then left becomes right to keep the adjustment logic coherent
        type %in% types & t == "prop" & d == "left" ~ "right",
        TRUE ~ d
      )
    )
}
