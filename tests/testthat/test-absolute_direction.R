test_that("tri_absolute_direction() converts directions correctly", {
  dat <- triangulate::dat_bias %>%
    tri_to_long() %>%
    tri_absolute_direction()

  expect_true("d" %in% names(dat))
  expect_true(all(dat$d %in% c("left", "right", "unpredictable", "none", "")))
})

test_that("tri_absolute_direction_invert() reverses directions correctly", {
  dat <- triangulate::dat_bias %>%
    tri_to_long() %>%
    tri_absolute_direction() %>%
    tri_absolute_direction_invert()

  expect_true("d" %in% names(dat))
})
