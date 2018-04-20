context("unpack_integers")

test_that("cat_ids", {

  df <- data_frame(cat_ids = c("c(1, 3)", "c(1:4)"))
  expected <- data_frame(cat_ids = as.integer(c(1, 3, 1, 2, 3, 4)))

  expect_identical(unpack_integers(df, var_name = "cat_ids"), expected)

})
