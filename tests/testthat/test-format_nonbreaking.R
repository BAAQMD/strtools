context("format_nonbreaking")

test_that("spaces", {

  x <- c("foo bar baz")

  expect_equal(
    format_nonbreaking(x),
    str_c("foo", "bar", "baz", sep = "\u00A0"))

})

test_that("hyphens", {

  x <- c("foo-bar-baz")

  expect_equal(
    format_nonbreaking(x),
    str_c("foo", "bar", "baz", sep = "\u2011"))

})
