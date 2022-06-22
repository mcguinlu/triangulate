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

  sort <- gsub("d","",dat$domain) %>%
    as.numeric() %>%
    max() %>%
    seq_len() %>%
    Map(function(x){
      c(paste0("d",x,"j"),
        paste0("d",x,"t"),
        paste0("d",x,"d"))
    },.) %>% unlist()

  dat <- dat %>%
    tidyr::pivot_wider(
      names_glue = "{domain}{.value}",
      names_from = "domain",
      values_from = dplyr::matches("j$|^t$|^d$")
    ) %>%
    dplyr::select(!dplyr::matches(sort), dplyr::matches(sort)) %>%
    tidyr::as_tibble() %>%
    return()
}
