test_that("format_digits()", {

  x <- c(1, 1.2, 45.67)

  expect_equal(
    format_digits(x, 1),
    c("1.0", "1.2", "45.7"))

})

test_that("format_digits() omits trailing decimal", {

  x <- c(1:10)
  expect_equal(format_digits(x, 0), as.character(round(x)))

})

test_that("format_digits() respects `na` argument", {

  x <- c(1, NA, 2)
  expect_equal(format_digits(x), c("1", NA, "2"))
  expect_equal(format_digits(x, na = "-"), c("1", "-", "2"))
  expect_equal(format_digits(x, na = ""), c("1", "", "2"))

})


