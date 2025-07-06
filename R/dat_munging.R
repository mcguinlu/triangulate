#' Convert data from wide to long format
#'
#' Reshapes domain-level bias columns from wide format (e.g., d1j, d1t, d1d) to long format.
#' Assumes columns follow naming pattern like d1j, d1t, d1d, d2j, etc.
#'
#' @param dat A data frame in wide format
#'
#' @return A long-format data frame with columns: domain, j, t, d
#' @export
tri_to_long <- function(dat) {
  # Check expected columns exist
  if (!any(grepl("^d[0-9]+(j|t|d)$", names(dat)))) {
    stop("tri_to_long(): No domain-level columns found (e.g. d1j, d1t, d1d).")
  }

  dat %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("d[0-9]+(d|j|t)"),
      names_to = c("domain", ".value"),
      names_pattern = "(d[0-9]+)(d|j|t)"
    ) %>%
    dplyr::mutate(dplyr::across(c(j, d, t), ~ stringr::str_to_lower(.x))) %>%
    dplyr::select(-dplyr::matches("^(j|t|d)$"), j, t, d) %>%
    return()
}



#' Convert data from long to wide format
#'
#' Reshapes domain-level bias columns from long format to wide format (e.g., d1j, d1t, d1d).
#' Assumes long format with columns 'domain', 'j', 't', 'd'.
#'
#' @param dat A long-format triangulate dataset
#'
#' @return A wide-format data frame
#' @export
tri_to_wide <- function(dat) {
  if (!"domain" %in% names(dat)) {
    stop("tri_to_wide(): Expected column 'domain' not found in dataset.")
  }

  # Get number of domains and expected output column order
  max_domain <- suppressWarnings(max(as.numeric(gsub("d", "", dat$domain)), na.rm = TRUE))
  if (is.na(max_domain)) {
    stop("tri_to_wide(): Failed to parse domain numbers. Are domain labels in the form 'd1', 'd2', etc.?")
  }

  sort <- unlist(Map(function(x) {
    c(paste0("d", x, "j"), paste0("d", x, "t"), paste0("d", x, "d"))
  }, seq_len(max_domain)))

  dat %>%
    tidyr::pivot_wider(
      names_glue = "{domain}{.value}",
      names_from = "domain",
      values_from = dplyr::matches("j$|^t$|^d$")
    ) %>%
    dplyr::select(-dplyr::matches(sort), dplyr::matches(sort)) %>%
    dplyr::as_tibble()
}

