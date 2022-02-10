#' Title
#'
#' @param data
#' @param values
#' @param common
#'
#' @return
#' @export

tri_append_bias <- function(data, values, common = T) {

  values <- values %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(starts_with("bias_"), as.double)) %>%
    dplyr::mutate(dplyr::across(c(domain,j), stringr::str_to_lower))

  # Define criteria on which to join with values#
  # If common = T, means that the values of Serious/Moderate are consistent across domains
  if (common == T) {
    by = c("j")

    if ("domain" %in% colnames(values)) {
      values <- dplyr::select(values, -domain)
    }

  } else {
    by = c("domain","j")
  }

  data %>%
    janitor::clean_names() %>%

    # Add basic values
    dplyr::left_join(values, by = by) %>%

    # Set values to 0 when judgement is NA
    # Most common when one tool has less domains than another
    dplyr::mutate(dplyr::across(dplyr::matches("bias_._add|bias_._prop"),
                  ~ dplyr::case_when(is.na(j) == T ~ 0,
                              j == "NA" ~ 0,
                              T ~ .))) %>%

    # Set additive biases mean/var to 0 when type is "prop"
    dplyr::mutate(dplyr::across(dplyr::matches("bias_._add"),
                  ~ dplyr::case_when(t == "prop" ~ 0,
                              T ~ .))) %>%

    # Set proportional biases mean/var to 0 when type is "add"
    dplyr::mutate(dplyr::across(dplyr::matches("bias_._prop"),
                  ~ dplyr::case_when(t == "add" ~ 0,
                              T ~ .))) %>%

    # Adjust signs of bias based on d (direction)
    # Where d is unpredictable, set mean to 0, but keep variance estimate
    dplyr::mutate(
      bias_m_add = dplyr::case_when(d == "left" ~ bias_m_add * -1,
                             d == "unpredictable" ~ 0,
                             T ~ bias_m_add),
      bias_m_prop = dplyr::case_when(d == "left" ~ bias_m_prop * -1,
                              d == "unpredictable" ~ 0,
                              T ~ bias_m_prop)
    ) %>%

    return()
}


#' Title
#'
#' @param data
#' @param values
#' @param common
#'
#' @return
#' @export

tri_append_indirect <- function(data, values, common = T) {

  # Define criteria on which to join with values#
  # If common = T, means that the values of Serious/Moderate are consistent across domains
  if (common == T) {
    by = c("j")

    if ("domain" %in% colnames(values)) {
      values <- dplyr::select(values, -domain)
    }

  } else {
    by = c("domain","j")
  }

  data %>%
    # Add basic values
    dplyr::left_join(values, by = by) %>%

    # Set values to 0 when judgement is NA
    # Most common when one tool has less domains than another
    dplyr::mutate(dplyr::across(dplyr::matches("ind_._add|ind_._prop"),
                  ~ dplyr::case_when(is.na(j) == T ~ 0,
                              j == "NA" ~ 0,
                              T ~ .))) %>%

    # Set additive biases mean/var to 0 when type is "prop"
    dplyr::mutate(dplyr::across(dplyr::matches("ind_._add"),
                  ~ dplyr::case_when(t == "prop" ~ 0,
                              T ~ .))) %>%

    # Set proportional biases mean/var to 0 when type is "add"
    dplyr::mutate(dplyr::across(dplyr::matches("ind_._prop"),
                  ~ dplyr::case_when(t == "add" ~ 0,
                              T ~ .))) %>%

    # Adjust signs of bias based on d (direction)
    # Where d is unpredictable, set mean to 0, but keep variance estimate
    dplyr::mutate(
      ind_m_add = dplyr::case_when(d == "left" ~ ind_m_add * -1,
                            d == "unpredictable" ~ 0,
                            T ~ ind_m_add),
      ind_m_prop = dplyr::case_when(d == "left" ~ ind_m_prop * -1,
                             d == "unpredictable" ~ 0,
                             T ~ ind_m_prop)
    ) %>%

    return()
}
