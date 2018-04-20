context("str_begins")

test_that("character", {
  x <- c("C22240098", "C22250098", "C20240189", "G1003260")
  expect_identical(str_begins(x, "G"), c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(str_begins(x, "C22"), c(TRUE, TRUE, FALSE, FALSE))
})
