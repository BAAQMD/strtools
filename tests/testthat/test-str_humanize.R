context("str_humanize")

expect_identical(
  str_humanize(c("bear", "cat", "dog")),
  c("bear", "cat", "dog"))

x <- c(123456789.01, 0.996, 0.003, 0.007, 0.000001, 0)

test_that("digits", {
  expect_identical(
    str_humanize(x, digits = 3),
    c("123,456,789.010", "0.996", "0.003", "0.007", "0.000", "0.000"))
})

test_that("tiny", {
  expect_equal(
    str_humanize(x, digits = 3, tiny = "-"),
    c("123,456,789.010", "0.996", "0.003", "0.007", "-", "0.000"))
})

test_that("zero", {
  expect_equal(
    str_humanize(x, digits = 2, tiny = "-", zero = ""),
    c("123,456,789.01", "1.00", "-", "0.01", "-", ""))
})
