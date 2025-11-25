test_that("tri_to_long and tri_to_wide retain structure", {
  dat_long <- triangulate::dat_bias %>%
    tri_to_long()

  expect_true("domain" %in% names(dat_long))
  expect_true("j" %in% names(dat_long))
  expect_true("d" %in% names(dat_long))
  expect_true("t" %in% names(dat_long))

  dat_wide <- dat_long %>%
    tri_to_wide()

  expect_equal(nrow(dat_wide), nrow(triangulate::dat_bias))
})



