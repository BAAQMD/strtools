context("guess_digits")

test_that("simple", {

  expect_equal(guess_digits(56), 0)
  expect_equal(guess_digits(0.66), 2)
  expect_equal(guess_digits(0.667), 3)

})
