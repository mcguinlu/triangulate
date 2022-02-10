#' Title
#'
#' @param dat
#'
#' @return
#' @export
tri_to_long <- function(dat){

  dat %>%
    # Clean and covert to long format
    tidyr::pivot_longer(
      cols = matches("d[0-9]+(d|j|t)"),
      names_to = c("domain", ".value"),
      names_pattern = "(d[0-9]+)(d|j|t)"
    ) %>%
    dplyr::mutate(across(c(j, d, t), stringr::str_to_lower)) %>%
    # Order columns appropriately
    dplyr::select(!matches("^(j|t|d)$"), "j", "t", "d") %>%
    return()
}

#' Title
#'
#' @param dat
#'
#' @return
#' @export
tri_to_wide <- function(dat) {
  dat %>%
    tidyr::pivot_wider(
      names_glue = "{domain}{.value}",
      names_from = "domain",
      values_from = matches("j$|^t$|^d$")
    ) %>%
    return()
}
