test_that("tri_plot_bias_direction runs without error on CHD example", {
  skip_on_cran()  # Skip on CRAN (plotting tests are fragile)
  skip_if_not_installed("metafor")

  # Load example data (produced from prep pipeline)
  data_path <- system.file("extdata/RoB_betaCarotene_CHD_v3.Rdata", package = "triangulate")
  load(data_path)  # loads `dat_CHDbias`


  expect_true(exists("dat_CHDbias"))

  dat_long <- dat_CHDbias %>%
    tri_to_long() %>%
    tri_absolute_direction()

  dat_bias <- dat_long %>%
    tri_append_bias(triangulate::dat_bias_values)

  dat_ind <- dat_long %>%
    tri_append_indirect(triangulate::dat_ind_values)

  dat_input <- tri_prep_data(dat_bias, dat_ind)  # Prepares merged dataset
  dat_final <- tri_calculate_adjusted_estimates(dat_input)


  dat_for_plot <- dplyr::left_join(dat_CHDbias, dat_final[, c("result_id", "yi_adj", "vi_adj")])

  # Create temporary PNG output to confirm plot runs
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 800, height = 600)

  expect_error(
    tri_plot_bias_direction(dat_for_plot, title = "Test Bias Plot"),
    NA  # Expect no error
  )

  dev.off()
  expect_true(file.exists(tmp))
  file.remove(tmp)
})


