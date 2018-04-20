context("format_SI")

test_that("small integers", {

  expect_identical(format_SI(1), "1")
  expect_identical(format_SI(9:11), c("9", "10", "11"))

})

test_that("dynamic range", {

  expect_identical(
    format_SI(c(33, 333, 3333, 33333333)),
    c("33.0", "333.0", "3.3k", "33.3M"))

})
