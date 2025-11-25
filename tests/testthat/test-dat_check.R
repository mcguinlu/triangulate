test_that("tri_dat_check() passes with valid input", {
  expect_message(tri_dat_check(triangulate::dat_bias), "All expected columns are present")
})

test_that("tri_dat_check() triggers error on missing columns", {
  dat_missing <- triangulate::dat_bias[, -1]  # remove result_id
  expect_error(tri_dat_check(dat_missing), "tri_dat_check\\(\\): The following required column\\(s\\) are missing: result_id")
})
