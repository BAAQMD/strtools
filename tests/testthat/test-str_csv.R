context("str_csv")

test_that("numeric vector", {

  expect_identical(
    str_csv(c(1:3)),
    "1, 2, 3")

})

test_that("no arguments", {

  expect_identical(
    str_csv(),
    character(0))

})

test_that("multiple vectors", {

  expect_identical(
    str_csv(1:3, "A", c("B", "C")),
    "1, 2, 3, A, B, C")

})

