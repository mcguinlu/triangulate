test_that("Expect error if a column is missing", {
  expect_error(tri_dat_check(dat_bias[,2:26]),"Column 'result_id' is missing")
})
