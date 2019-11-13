context("format_percent_change")

test_that("zero", {
  expect_equal(format_percent_change(1), "0%")
  expect_equal(format_percent_change(1, zero = "0"), "0")
})

test_that("NA", {
  expect_equal(format_percent_change(NA), "")
  expect_equal(format_percent_change(NA, na = "NA"), "NA")
})

test_that("increase", {
  expect_equal(format_percent_change(1.125), "+12%")
  expect_equal(format_percent_change(1.125, digits = 1), "+12.5%")
})

test_that("decrease", {
  expect_equal(format_percent_change(0.875), "-12%")
})

test_that("mixed", {
  expect_equal(
    format_percent_change(c(1.125, 1.000, NA, 0.875)),
    c("+12%", "0%", "", "-12%"))
})
