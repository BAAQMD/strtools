context("unpack_integers")

library(dplyr)
library(purrr)

input_data <- data_frame(cat_id = c("c(1, 3)", "c(1:4)"))
expected <- data_frame(cat_id = as.integer(c(1, 3, 1:4)))

test_that("explicit var_name", {

  expect_identical(
    unpack_integers(input_data, var_name = "cat_id"),
    expected)

})
