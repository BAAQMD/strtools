context("str_quote")

test_that("simple", {

  result <- str_quote(1:3)
  expected <- c('"1"', '"2"', '"3"')
  expect_equal(result, expected)

})
