#' Calculate adjusted effect estimates and variances
#'
#' Applies bias and indirectness corrections to effect estimates `yi` and variances `vi`.
#' Requires additive and proportional bias/indirectness inputs.
#'
#' @param dat Data in long format with bias and indirectness columns
#'
#' @return Data with `yi_adj` and `vi_adj` columns added
#'
#' @export
tri_calculate_adjusted_estimates <- function(dat) {

  dat <- dat %>%
    dplyr::mutate(
      # Avoid divide-by-zero
      scale_factor = propimn * propemn,
      scale_factor = dplyr::if_else(scale_factor == 0, NA_real_, scale_factor),

      yi_adj = (yi - addimn - propimn * addemn) / scale_factor,

      vi_adj = (
        ((propimn^2 + propivar) * (propevar * yi_adj^2 + addevar)) +
          (propivar * (propemn * yi_adj + addemn)^2) +
          addivar + vi
      ) / (scale_factor^2)
    ) %>%
    # Replace Inf or NaN with NA and warn
    dplyr::mutate(
      yi_adj = dplyr::if_else(is.finite(yi_adj), yi_adj, NA_real_),
      vi_adj = dplyr::if_else(is.finite(vi_adj), vi_adj, NA_real_)
    )

  return(dat)
}

#' Prepare bias and indirectness data for adjustment
#'
#' Aggregates domain-level prior data and calculates total adjustment values
#' per study result. Supports additive and proportional adjustments.
#'
#' @param dat_bias Data with bias priors (long format)
#' @param dat_ind Data with indirectness priors (long format)
#'
#' @return Data with yi/vi and adjustment components ready for adjustment
#'
#' @export
tri_prep_data <- function(dat_bias, dat_ind) {

  i_add <- dat_bias %>%
    dplyr::group_by(result_id) %>%
    dplyr::summarise(
      addimn = sum(bias_m_add, na.rm = TRUE),
      addivar = sum(bias_v_add, na.rm = TRUE),
      .groups = "drop"
    )

  e_add <- dat_ind %>%
    dplyr::group_by(result_id) %>%
    dplyr::summarise(
      addemn = sum(ind_m_add, na.rm = TRUE),
      addevar = sum(ind_v_add, na.rm = TRUE),
      .groups = "drop"
    )

  i_prop <- dat_bias %>%
    dplyr::group_by(result_id) %>%
    dplyr::summarise(
      sumlogmn = sum(bias_m_prop, na.rm = TRUE),
      sumlogvr = sum(bias_v_prop, na.rm = TRUE),
      propimn = exp(sumlogmn + sumlogvr / 2),
      propivar = exp(2 * sumlogmn + sumlogvr) * (exp(sumlogvr) - 1),
      .groups = "drop"
    )

  e_prop <- dat_ind %>%
    dplyr::group_by(result_id) %>%
    dplyr::summarise(
      sumlogmn = sum(ind_m_prop, na.rm = TRUE),
      sumlogvr = sum(ind_v_prop, na.rm = TRUE),
      propemn = exp(sumlogmn + sumlogvr / 2),
      propevar = exp(2 * sumlogmn + sumlogvr) * (exp(sumlogvr) - 1),
      .groups = "drop"
    )

  dat_final <- dat_bias %>%
    dplyr::distinct(result_id, study, yi, vi) %>%
    dplyr::left_join(i_add, by = "result_id") %>%
    dplyr::left_join(i_prop, by = "result_id") %>%
    dplyr::left_join(e_add, by = "result_id") %>%
    dplyr::left_join(e_prop, by = "result_id") %>%
    tri_calculate_adjusted_estimates()

  return(dat_final)
}


