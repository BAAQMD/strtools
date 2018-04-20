context("str_humanize")

x <- c(123456789.01, 0.996, 0.003, 0.007, 0.000001, 0)

test_that("default", {
  expect_identical(
    str_humanize(x),
    c("123,460,000", "0", "0", "0", "0", "0"))
})

test_that("digits", {
  expect_identical(
    str_humanize(x, digits = 3),
    c("123,456,789.010", "0.996", "0.003", "0.007", "0.000", "0.000"))
})

test_that("digits and tiny", {
  expect_identical(
    str_humanize(x, digits = 3, tiny = "-"),
    c("123,456,789.010", "0.996", "0.003", "0.007", "-", "0.000"))
})

test_that("digits and tiny and zero", {
  expect_identical(
    str_humanize(x, digits = 3, tiny = "-", zero = ""),
    c("123,456,789.010", "0.996", "0.003", "0.007", "-", ""))
})
