test_that("tri_append_bias() appends values without error", {
  dat <- triangulate::dat_bias %>%
    tri_to_long() %>%
    tri_absolute_direction() %>%
    tri_append_bias(values = triangulate::dat_bias_values)

  expect_true(all(c("bias_m_add", "bias_v_add", "bias_m_prop", "bias_v_prop") %in% names(dat)))
  expect_type(dat$bias_m_add, "double")
})

test_that("tri_append_indirect() appends values without error", {
  dat <- triangulate::dat_ind %>%
    tri_to_long() %>%
    tri_absolute_direction() %>%
    tri_append_indirect(values = triangulate::dat_ind_values)

  expect_true(all(c("ind_m_add", "ind_v_add", "ind_m_prop", "ind_v_prop") %in% names(dat)))
  expect_type(dat$ind_m_add, "double")
})

