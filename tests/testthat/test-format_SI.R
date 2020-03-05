context("format_SI")

test_that("small integers", {

  expect_identical(format_SI(1), "1")
  expect_identical(format_SI(9:11), c("9", "10", "11"))

})

test_that("dynamic range", {

  expect_identical(
    format_SI(c(33, 333, 3333, 33333333)),
    c("33.0", "333.0", "3.3k", "33.3M"))

  expect_identical(
    format_SI(seq(0, 1.2, length.out = 7) * 1000),
    c("0.0", "200.0", "400.0", "600.0", "800.0", "1000.0", "1.2k"))

  expect_identical(
    format_SI(seq(0, 1.2, length.out = 7) * 1000),
    format_SI(seq(0, 1.2, length.out = 7) * 1000, fixed = FALSE))

})

test_that("fixed range", {

  expect_identical(
    format_SI(seq(0, 1.2, length.out = 7) * 1000, fixed = TRUE),
    c("0.0k", "0.2k", "0.4k", "0.6k", "0.8k", "1.0k", "1.2k"))

})
