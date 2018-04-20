context("pack_integers")

test_that("non-adjacent", {
  x <- c(-1, 2, 9)
  expect_equal(pack_integers(x), "c(-1, 2, 9)")
})

test_that("simple range", {
  x <- c(1, 2, 3)
  expect_equal(pack_integers(x), "c(1:3)")
})

test_that("edges", {
  x <- c(1, 3, 4:7, 9)
  expect_equal(pack_integers(x), "c(1, 3:7, 9)")
})
