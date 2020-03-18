context("format_qty")

test_that("<1 order of magnitude", {

  x <- seq(0, 0.75, length.out = 4)

  expect_equal(
    format_qty(x * 1e0),
    c("0.00", "0.25", "0.50", "0.75"))

  expect_equal(
    format_qty(x * 1e1),
    c("0.0", "2.5", "5.0", "7.5"))

  expect_equal(
    format_qty(x * 1e2),
    c("0", "25", "50", "75"))

  expect_equal(
    format_qty(x * 1e3),
    c("0.00k", "0.25k", "0.50k", "0.75k"))

  expect_equal(
    format_qty(x * 1e4),
    c("0.0k", "2.5k", "5.0k", "7.5k"))

  expect_equal(
    format_qty(x * 1e5),
    c("0k", "25k", "50k", "75k"))
  expect_equal(
    format_qty(x * 1e6),
    c("0.00M", "0.25M", "0.50M", "0.75M"))

  expect_equal(
    format_qty(x * 1e7),
    c("0.0M", "2.5M", "5.0M", "7.5M"))

})

test_that("engineering (>1 order of magnitude)", {

  x <- seq(0, 1.25, length.out = 6)

  expect_equal(
    format_qty(x * 1e0),
    c("0.00", "0.25", "0.50", "0.75", "1.00", "1.25"))

  expect_equal(
    format_qty(x * 1e1),
    c("0.0", "2.5", "5.0", "7.5", "10.0", "12.5"))

  expect_equal(
    format_qty(x * 1e2),
    c("0", "25", "50", "75", "100", "125"))

  expect_equal(
    format_qty(x * 1e3),
    c("0.00k", "0.25k", "0.50k", "0.75k", "1.00k", "1.25k"))

  expect_equal(
    format_qty(x * 1e4),
    c("0.0k", "2.5k", "5.0k", "7.5k", "10.0k", "12.5k"))

  expect_equal(
    format_qty(x * 1e5),
    c("0k", "25k", "50k", "75k", "100k", "125k"))

  expect_equal(
    format_qty(x * 1e6),
    c("0.00M", "0.25M", "0.50M", "0.75M", "1.00M", "1.25M"))

  expect_equal(
    format_qty(x * 1e7),
    c("0.0M", "2.5M", "5.0M", "7.5M", "10.0M", "12.5M"))

})
