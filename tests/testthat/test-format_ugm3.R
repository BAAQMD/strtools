test_that("format_ugm3() works", {

  expect_equal(
    format_ugm3(1.23456),
    "1.235")

  expect_equal(
    format_ugm3(1.23456, digits = 4),
    "1.2346")

  expect_equal(
    format_ugm3(1.23456, digits = 1, sign = TRUE),
    "+1.2")

})
