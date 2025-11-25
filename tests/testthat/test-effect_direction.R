test_that("tri_swap_effect_direction inverts direction for specified types", {
  dat <- triangulate::dat_bias %>%
    tri_to_long()

  # Make a copy with known type and direction
  dat$type <- "obs"
  dat$t <- "prop"
  dat$d <- "right"

  dat_flipped <- tri_swap_effect_direction(dat, types = "obs")

  expect_true(all(dat_flipped$d == "left"))
  expect_true(all(dat_flipped$yi == -dat$yi))
})

