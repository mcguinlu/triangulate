test_that("tri_prep_data() and tri_calculate_adjusted_estimates() run correctly", {
  dat_bias_prep <- triangulate::dat_bias %>%
    tri_to_long() %>%
    tri_absolute_direction() %>%
    tri_append_bias(triangulate::dat_bias_values)

  dat_ind_prep <- triangulate::dat_ind %>%
    tri_to_long() %>%
    tri_absolute_direction() %>%
    tri_append_indirect(triangulate::dat_ind_values)

  result <- tri_prep_data(dat_bias_prep, dat_ind_prep)

  expect_true("yi_adj" %in% colnames(result))
  expect_true("vi_adj" %in% colnames(result))
})

