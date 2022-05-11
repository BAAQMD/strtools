test_that("integers work", {

  expect_equal(-10:10, parse_numeric(-10:10))

})

test_that("decimals work", {

  x <- 1e6 * rnorm(10)

  expect_equal(
    parse_numeric(format_count(x)),
    round(x))

  expect_equal(
    parse_numeric(format_count(x, digits = 2)),
    round(x, digits = 2))

})

test_that("scientific notation works", {

  expect_equal(
    parse_numeric("1.4e-3"),
    0.0014)

  expect_equal(
    parse_numeric("1.4E+3"),
    1400)

  expect_equal(
    parse_numeric("1.4e03"),
    1400)

})
