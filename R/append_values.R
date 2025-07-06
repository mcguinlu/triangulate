#' Append bias priors to bias dataset
#'
#' Joins the appropriate prior parameters to a long-format bias dataset, based on
#' the risk-of-bias judgment (j), direction (d), and type (t).
#'
#' @param dat A bias dataset in long format (must include j, d, t, domain, result_id)
#' @param values A data frame of prior distributions (e.g., from dat_bias_values)
#' @param common Should priors apply across all domains (TRUE), or vary by domain?
#'
#' @return Dataset with numeric bias prior values appended
#' @export
tri_append_bias <- function(dat, values, common = TRUE) {

  # Validate direction values
  allowed_d <- c("left", "right", "unpredictable", "none")
  bad_d <- setdiff(unique(dat$d), allowed_d)
  if (length(bad_d) > 0) {
    stop("tri_append_bias(): Found unexpected 'd' values: ", paste(bad_d, collapse = ", "),
         ". Did you forget to run tri_absolute_direction()?")
  }

  # Validate column presence
  required_cols <- c("j", "d", "t", "domain", "result_id")
  missing_cols <- setdiff(required_cols, colnames(dat))
  if (length(missing_cols) > 0) {
    stop("tri_append_bias(): Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Clean and format priors table
  values <- values %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("bias_"), as.double)) %>%
    dplyr::mutate(dplyr::across(c(domain, j), stringr::str_to_lower))

  # Join logic
  by <- if (common) {
    values <- dplyr::select(values, -any_of("domain"))
    c("j")
  } else {
    c("domain", "j")
  }

  dat %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%

    dplyr::left_join(values, by = by) %>%

    # Set bias values to 0 where judgment is missing
    dplyr::mutate(dplyr::across(dplyr::matches("bias_._add|bias_._prop"),
                                ~ dplyr::case_when(is.na(j) | j == "na" ~ 0, TRUE ~ .))) %>%

    # Drop additive values if t == "prop"
    dplyr::mutate(dplyr::across(dplyr::matches("bias_._add"),
                                ~ dplyr::case_when(t == "prop" ~ 0, TRUE ~ .))) %>%

    # Drop proportional values if t == "add"
    dplyr::mutate(dplyr::across(dplyr::matches("bias_._prop"),
                                ~ dplyr::case_when(t == "add" ~ 0, TRUE ~ .))) %>%

    # Adjust direction
    dplyr::mutate(
      bias_m_add = dplyr::case_when(d == "left" ~ -bias_m_add,
                                    d == "unpredictable" ~ 0,
                                    TRUE ~ bias_m_add),
      bias_m_prop = dplyr::case_when(d == "left" ~ -bias_m_prop,
                                     d == "unpredictable" ~ 0,
                                     TRUE ~ bias_m_prop)
    )
}

#' Append indirectness priors to dataset
#'
#' Similar to tri_append_bias(), but operates on indirectness judgments and priors
#'
#' @param dat A long-format indirectness dataset
#' @param values A dataframe of indirectness priors (e.g., dat_ind_values)
#' @param common Should priors apply across all domains (TRUE), or vary by domain?
#'
#' @return Dataset with indirectness prior values appended
#' @export
tri_append_indirect <- function(dat, values, common = TRUE) {

  by <- if (common) {
    values <- dplyr::select(values, -any_of("domain"))
    c("j")
  } else {
    c("domain", "j")
  }

  dat %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(c(j, d, t), stringr::str_to_lower)) %>%
    dplyr::left_join(values, by = by) %>%

    dplyr::mutate(dplyr::across(dplyr::matches("ind_._add|ind_._prop"),
                                ~ dplyr::case_when(is.na(j) | j == "na" ~ 0, TRUE ~ .))) %>%

    dplyr::mutate(dplyr::across(dplyr::matches("ind_._add"),
                                ~ dplyr::case_when(t == "prop" ~ 0, TRUE ~ .))) %>%

    dplyr::mutate(dplyr::across(dplyr::matches("ind_._prop"),
                                ~ dplyr::case_when(t == "add" ~ 0, TRUE ~ .))) %>%

    dplyr::mutate(
      ind_m_add = dplyr::case_when(d == "left" ~ -ind_m_add,
                                   d == "unpredictable" ~ 0,
                                   TRUE ~ ind_m_add),
      ind_m_prop = dplyr::case_when(d == "left" ~ -ind_m_prop,
                                    d == "unpredictable" ~ 0,
                                    TRUE ~ ind_m_prop)
    )
}

