context("guess_digits")

test_that("single value", {

  expect_equal(guess_digits(-0.01), 2)
  expect_equal(guess_digits(0.01), 2)
  expect_equal(guess_digits(0.1), 1)
  expect_equal(guess_digits(0), 0)
  expect_equal(guess_digits(1), 0)
  expect_equal(guess_digits(10), 0)
  expect_equal(guess_digits(100), 0)
  expect_equal(guess_digits(-100), 0)

  expect_equal(guess_digits(56), 0)
  expect_equal(guess_digits(0.66), 2)
  expect_equal(guess_digits(0.667), 3)
  expect_equal(guess_digits(0.6678), 4)

})

test_that("vector", {

  expect_equal(guess_digits(c(1, 10, 100)), 0)
  expect_equal(guess_digits(c(0, 2.5, 5, 7.5)), 1)
  expect_equal(guess_digits(c(0, 2.5, 5, 7.54)), 2)

})
