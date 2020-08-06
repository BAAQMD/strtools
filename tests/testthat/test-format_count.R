test_that("small numbers work", {

  expect_equal(
    format_count(c(1, 23, 456)),
    c("1", "23", "456"))

  expect_equal(format_count(765.432), "765")
  expect_equal(format_count(765.432, digits = 2), "765.43")

})

test_that("larger numbers work", {

  expect_equal(
    format_count(c(1000, 23456, 456789)),
    c("1,000", "23,456", "456,789"))

  expect_equal(format_count(765234.432), "765,234")
  expect_equal(format_count(765234.432, digits = 2), "765,234.43")

})
