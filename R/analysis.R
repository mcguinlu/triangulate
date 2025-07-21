#' Calculate Bias-Adjusted Estimates
#'
#' Computes adjusted effect estimates (`yi_adj`) and variances (`vi_adj`)
#' incorporating additive and proportional bias adjustments.
#'
#' @param dat A dataset prepared using `tri_prep_data()`, containing bias parameters
#' @return A tibble with added `yi_adj` and `vi_adj` columns
#' @export
tri_calculate_adjusted_estimates <- function(dat) {
  dat <- dat %>%
    dplyr::mutate(
      # Compute scale factor
      scale_factor = propimn * propemn
    )

  # Warn if any scale factors are exactly 0
  if (any(dat$scale_factor == 0, na.rm = TRUE)) {
    warning("Some values of propimn * propemn equal 0. Check for zero proportional biases - is this correct?")
  }

  dat <- dat %>%
    dplyr::mutate(
      # Calculate adjusted effect
      yi_adj = (yi - addimn - propimn * addemn) / scale_factor,

      # Calculate adjusted variance
      vi_adj = (
        ((propimn^2 + propivar) * (propevar * yi_adj^2 + addevar)) +
          (propivar * (propemn * yi_adj + addemn)^2) +
          addivar + vi
      ) / (scale_factor^2),

      # Replace Inf or NaN with NA (defensive)
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
    dplyr::distinct(result_id, study, type, yi, vi) %>%
    dplyr::left_join(i_add, by = "result_id") %>%
    dplyr::left_join(i_prop, by = "result_id") %>%
    dplyr::left_join(e_add, by = "result_id") %>%
    dplyr::left_join(e_prop, by = "result_id") %>%
    tri_calculate_adjusted_estimates()

  return(dat_final)
}


