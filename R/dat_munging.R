#' Title
#'
#' @param dat Data in wide format
#'
#' @export
tri_to_long <- function(dat){

  dat %>%
    # Clean and covert to long format
    tidyr::pivot_longer(
      cols = dplyr::matches("d[0-9]+(d|j|t)"),
      names_to = c("domain", ".value"),
      names_pattern = "(d[0-9]+)(d|j|t)"
    ) %>%
    dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%
    # Order columns appropriately
    dplyr::select(!dplyr::matches("^(j|t|d)$"), "j", "t", "d") %>%
    return()
}

#' Title
#'
#' @param dat Data in long format
#'
#' @export
tri_to_wide <- function(dat) {
  dat <- dat %>%
    tidyr::pivot_wider(
      names_glue = "{domain}{.value}",
      names_from = "domain",
      values_from = dplyr::matches("j$|^t$|^d$")
    ) %>%
    return()


  # Get columns back into proper order
  # It is not pretty but it works
  # TODO Clean this up
  dat2 <- select(dat, dplyr::matches("^d.j$|^d.t$|^d.d$"))

  n_domains <- ncol(dat2) / 3

  dat3 <- select(dat, !dplyr::matches("^d.j$|^d.t$|^d.d$"))

  for (i in 1:n_domains) {
    dat3 <- cbind(dat3, dat2[seq(i, ncol(dat2), by = n_domains)])

  }

  return(tidyr::as_tibble(dat3))
}
