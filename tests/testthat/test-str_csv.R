context("str_csv")

test_that("numeric vector", {

  expect_identical(
    str_csv(c(1:3)),
    "1, 2, 3")

})

test_that("na.rm", {

  x <- c(1, NA, 99)

  expect_identical(
    str_csv(x),
    "1, NA, 99")

  expect_identical(
    str_csv(x, na.rm = TRUE),
    "1, 99")

})

test_that("no arguments", {

  expect_identical(
    str_csv(),
    "")

})

test_that("multiple vectors", {

  expect_identical(
    str_csv(1:3, "A", c("B", "C")),
    "1, 2, 3, A, B, C")

})

