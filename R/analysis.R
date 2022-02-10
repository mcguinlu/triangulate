#' Title
#'
#' @param dat
#'
#' @return
tri_calculate_adjusted_estimates <- function(dat) {
  dat %>%
    dplyr::mutate(
      yi_adj = (yi - addimn - propimn * addemn) / (propimn * propemn),
      vi_adj = ((((propimn ^ 2) + propivar) * (propevar * (yi_adj ^
                                                             2) + addevar) + propivar * ((propemn * yi_adj + addemn) ^ 2) + addivar + vi
      ) / ((propimn * propemn) ^ 2))
    ) %>%
    return()

}

#' Title
#'
#' @param dat_bias Cleaned bias dataset
#' @param dat_ind Cleaned indirectness dataset
#' @param bias_values Distributions for bias
#' @param indirect_values Distributions for indirectness
#'
#' @return
#' @export
tri_prep_data <- function(dat_bias, dat_ind){

  i_add <- dat_bias %>%
    dplyr::group_by(result_id) %>%
    dplyr::summarise(addimn = sum(bias_m_add, na.rm = T),
              addivar = sum(bias_v_add, na.rm = T)) %>%
    dplyr::select(result_id, starts_with("add"))

  e_add <- dat_ind %>%
    dplyr::group_by(result_id) %>%
    dplyr::summarise(addemn = sum(ind_m_add, na.rm = T),
              addevar = sum(ind_v_add, na.rm = T)) %>%
    dplyr::select(result_id, starts_with("add"))

  i_prop <- dat_bias %>%
    dplyr::group_by(result_id) %>%
    dplyr::summarise(sumlogmn = sum(bias_m_prop, na.rm = T),
              sumlogvr = sum(bias_v_prop, na.rm = T),
              propimn = exp(sumlogmn+sumlogvr/2),
              propivar = (exp(2*sumlogmn+sumlogvr)*(exp(sumlogvr)-1))) %>%
    dplyr::select(result_id, starts_with("prop"))

  e_prop <- dat_ind %>%
    dplyr::group_by(result_id) %>%
    dplyr::summarise(sumlogmn = sum(ind_m_prop, na.rm = T),
              sumlogvr = sum(ind_v_prop, na.rm = T),
              propemn = exp(sumlogmn+sumlogvr/2),
              propevar = (exp(2*sumlogmn+sumlogvr)*(exp(sumlogvr)-1))) %>%
    dplyr::select(result_id, starts_with("prop"))

  dat_final <- dat_bias %>%
    dplyr::distinct(result_id, study, yi, vi) %>%
    dplyr::left_join(i_add) %>%
    dplyr::left_join(i_prop) %>%
    dplyr::left_join(e_add) %>%
    dplyr::left_join(e_prop) %>%
    tri_calculate_adjusted_estimates() %>%
    return()

}

