context("format_decimal")

test_that("NAs", {
  expect_equal(
    unclass(format_decimal(c(3.14, NA))),
    c("3.14", NA_character_))
})

is_int <- function (x) (x == round(x))

x <- c(52.5999984741211, 417.399993896484, 103.800003051758)

test_that("guess_digits", {
  expect_equal(guess_digits(x), 1)
})

test_that("defaults", {
  expect_equal(
    unclass(format_decimal(x)),
    c("52.6", "417.4", "103.8"))
})


