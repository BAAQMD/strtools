context("parse_integers")

test_that("scalar", {
  expect_identical(parse_integers("1"), list(1L))
})

test_that("vector", {
  expect_identical(parse_integers("c(2, 3)"), list(c(2L, 3L)))
})

test_that("range", {
  expect_identical(parse_integers("c(5:9)"), list(as.integer(5:9)))
  expect_identical(parse_integers("5:9"), list(as.integer(5:9)))
})

test_that("NA", {
  expect_identical(parse_integers("NA"), list(NA_integer_))
})

test_that("string", {
  expect_error(parse_integers("foo"), "Couldn't parse")
})
