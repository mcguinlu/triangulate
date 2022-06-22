dat <- robvis::data_bias_direction_raw %>%
  tri_to_long() %>%
  tri_absolute_direction() %>%
  tri_to_wide()


dat_quick <- robvis::data_bias_direction_raw %>%
  tri_absolute_direction_quick()

test_that("cleaned dimensions are correct", {
  expect_equal(nrow(dat), 20)
  expect_equal(ncol(dat), 27)

  expect_equal(nrow(dat), nrow(dat_quick))
  expect_equal(ncol(dat), ncol(dat_quick))
})
