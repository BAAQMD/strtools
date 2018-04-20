context("replace_zero")

x <- c(0, 1, 2, 0, 3)

test_that("default", {

  expect_equal(
    replace_zero(x),
    c(NA, 1, 2, NA, 3))

})

test_that("NaN", {

  expect_equal(
    replace_zero(x, NaN),
    c(NaN, 1, 2, NaN, 3))

  expect_equal(
    replace_zero(x, -999),
    c(-999, 1, 2, -999, 3))

})
