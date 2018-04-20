context("precision")

test_that("simple", {

  expect_identical(precision(1300), 1000)
  expect_identical(precision(1370.32), 1000)
  expect_identical(precision(13.89), 10)
  expect_identical(precision(1.3), 1)
  expect_identical(precision(1), 1)
  expect_identical(precision(0.5), 0.1)
  expect_identical(precision(0.05123), 0.01)

})
