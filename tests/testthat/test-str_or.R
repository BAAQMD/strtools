context("str_or")

test_that("zero items", {
  expect_equal(str_or(NULL), NULL)
  expect_equal(str_or(character(0)), character(0))
})

test_that("one item", {
  x <- c("apples")
  expect_equal(str_or(x), "apples")
})

test_that("two items", {
  x <- c("apples", "oranges")
  expect_equal(str_or(x), "apples or oranges")
})

test_that("three items", {
  x <- c("apples", "oranges", "pears")
  expect_equal(str_or(x), "apples, oranges, or pears")
})

test_that("semicolon", {
  x <- c("apples", "oranges", "pears")
  expect_equal(str_or(x, .sep=";"), "apples; oranges; or pears")
})
