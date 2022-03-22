context("str_and")

test_that("zero items", {
  expect_equal(str_and(NULL), NULL)
  expect_equal(str_and(character(0)), character(0))
})

test_that("one item", {
  x <- c("apples")
  expect_equal(str_and(x), "apples")
})

test_that("two items", {
  x <- c("apples", "oranges")
  expect_equal(str_and(x), "apples and oranges")
})

test_that("three items", {
  x <- c("apples", "oranges", "pears")
  expect_equal(str_and(x), "apples, oranges, and pears")
})

test_that("semicolon", {
  x <- c("apples", "oranges", "pears")
  expect_equal(str_and(x, .sep=";"), "apples; oranges; and pears")
})
