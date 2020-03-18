context("format_SI")

test_that("small integers", {

  expect_identical(format_SI(1, verbose = TRUE), "1")
  expect_identical(format_SI(9:11, verbose = TRUE), c("9", "10", "11"))

})

test_that("digits", {

  expect_equal(format_SI(pi, digits = 0), "3")
  expect_equal(format_SI(pi, digits = 1), "3.1")
  expect_equal(format_SI(pi, digits = 2), "3.14")
  expect_equal(format_SI(pi, digits = 3), "3.142")

  expect_identical(format_SI(1, digits = 0), "1")
  expect_identical(format_SI(1, digits = 1), "1.0")
  expect_identical(format_SI(1, digits = 2), "1.00")

  expect_identical(format_SI(9:11, digits = 0), c("9", "10", "11"))
  expect_identical(format_SI(9:11, digits = 4), c("9.0000", "10.0000", "11.0000"))

})

test_that("dynamic range", {

  expect_identical(
    format_SI(c(33, 333, 3333, 33333333)),
    c("33", "333", "3k", "33M"))

  expect_identical(
    format_SI(c(33, 333, 3333, 33333333), digits = 1),
    c("33.0", "333.0", "3.3k", "33.3M"))

  expect_identical(
    format_SI(seq(0, 1.2, length.out = 7) * 1000, digits = 1),
    c("0.0", "200.0", "400.0", "600.0", "800.0", "1.0k", "1.2k"))

  expect_identical(
    format_SI(seq(0, 1.2, length.out = 7) * 1000),
    format_SI(seq(0, 1.2, length.out = 7) * 1000, fixed = FALSE))

  expect_identical(
    format_SI(seq(0, 0.9, length.out = 4) / 10),
    c("0", "30m", "60m", "90m"))

  expect_identical(
    format_SI(seq(0, 0.9, length.out = 4)),
    c("0", "300m", "600m", "900m"))


})

test_that("fixed range", {

  expect_identical(
    format_SI(seq(0, 0.9, length.out = 4), fixed = TRUE),
    c("0m", "300m", "600m", "900m"))

  expect_identical(
    format_SI(seq(0, 1.2, length.out = 7) * 1000, fixed = TRUE),
    c("0.0k", "0.2k", "0.4k", "0.6k", "0.8k", "1.0k", "1.2k"))

  expect_identical(
    format_SI(seq(0, 1.2, length.out = 7) * 1000, fixed = TRUE, digits = 2),
    c("0.00k", "0.20k", "0.40k", "0.60k", "0.80k", "1.00k", "1.20k"))

})

test_that("engineering (<1 order of magnitude)", {

  x <- seq(0, 0.75, length.out = 4)

  expect_equal(
    format_SI(x * 1e0, fixed = TRUE, engineering = TRUE),
    c("0.00", "0.25", "0.50", "0.75"))

  expect_equal(
    format_SI(x * 1e1, fixed = TRUE, engineering = TRUE),
    c("0.0", "2.5", "5.0", "7.5"))

  expect_equal(
    format_SI(x * 1e2, fixed = TRUE, engineering = TRUE),
    c("0", "25", "50", "75"))

  expect_equal(
    format_SI(x * 1e3, fixed = TRUE, engineering = TRUE),
    c("0.00k", "0.25k", "0.50k", "0.75k"))

  expect_equal(
    format_SI(x * 1e4, fixed = TRUE, engineering = TRUE),
    c("0.0k", "2.5k", "5.0k", "7.5k"))

  expect_equal(
    format_SI(x * 1e5, fixed = TRUE, engineering = TRUE),
    c("0k", "25k", "50k", "75k"))

  expect_equal(
    format_SI(x * 1e6, fixed = TRUE, engineering = TRUE),
    c("0.00M", "0.25M", "0.50M", "0.75M"))

  expect_equal(
    format_SI(x * 1e7, fixed = TRUE, engineering = TRUE),
    c("0.0M", "2.5M", "5.0M", "7.5M"))

})

test_that("engineering (>1 order of magnitude)", {

  x <- seq(0, 1.25, length.out = 6)

  expect_equal(
    format_SI(x * 1e0, fixed = TRUE, engineering = TRUE),
    c("0.00", "0.25", "0.50", "0.75", "1.00", "1.25"))

  expect_equal(
    format_SI(x * 1e1, fixed = TRUE, engineering = TRUE),
    c("0.0", "2.5", "5.0", "7.5", "10.0", "12.5"))

  expect_equal(
    format_SI(x * 1e2, fixed = TRUE, engineering = TRUE),
    c("0", "25", "50", "75", "100", "125"))

  expect_equal(
    format_SI(x * 1e3, fixed = TRUE, engineering = TRUE),
    c("0.00k", "0.25k", "0.50k", "0.75k", "1.00k", "1.25k"))

  expect_equal(
    format_SI(x * 1e4, fixed = TRUE, engineering = TRUE),
    c("0.0k", "2.5k", "5.0k", "7.5k", "10.0k", "12.5k"))

  expect_equal(
    format_SI(x * 1e5, fixed = TRUE, engineering = TRUE),
    c("0k", "25k", "50k", "75k", "100k", "125k"))

  expect_equal(
    format_SI(x * 1e6, fixed = TRUE, engineering = TRUE),
    c("0.00M", "0.25M", "0.50M", "0.75M", "1.00M", "1.25M"))

  expect_equal(
    format_SI(x * 1e7, fixed = TRUE, engineering = TRUE),
    c("0.0M", "2.5M", "5.0M", "7.5M", "10.0M", "12.5M"))

})
