context("parse_percentage")

test_that("real values", {

  expect_equal(
    parse_percentage(c("12.0%", "3%", "-120.5%")),
    c(0.12, 0.03, -1.205))

})


test_that("missing or infinite values", {

  expect_equal(
    parse_percentage(c("12.3%", "NA", "None", "", "Inf")),
    c(0.123, NA, NA, NA, Inf))

})
