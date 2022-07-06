test_that("format_range() works", {

  expect_error(
    format_range(c(NA, 10)),
    "NA detected")

  expect_equal(
    format_range(1:10),
    "1–10")

  expect_equal(
    format_range(c(9, 2, 5)),
    "2–9")

  expect_equal(
    format_range(1:10, sep = " to "),
    "1 to 10")

})
