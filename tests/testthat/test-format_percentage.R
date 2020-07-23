context("format_percentage")

test_that("simple", {
  expect_identical(format_percentage(1.00), "100%")
  expect_identical(format_percentage(2/3), "67%")
})

test_that("digits", {
  expect_identical(format_percentage(2/3, digits = 3), "66.667%")
})

test_that("trailing zero", {
  expect_identical(format_percentage(0.01, digits = 1), "1.0%")
})
