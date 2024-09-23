test_that("rename.default() works on numeric vector", {

  x <- 1:10L
  names(x) <- LETTERS[1:10]

  expected <- x
  i <- which(names(expected) == "G")
  names(expected)[i] <- "Foo"

  expect_equal(
    rename(x, Foo = G),
    expected)

})

test_that("rename.default() works on character vector", {

  x <- as.character(1:10)
  names(x) <- LETTERS[1:10]

  expected <- x
  i <- which(names(expected) == "H")
  names(expected)[i] <- "Bar"

  expect_equal(
    rename(x, Bar = H),
    expected)

})

test_that("rename.default() works on simple list", {

  x <- as.list(1:10)
  names(x) <- LETTERS[1:10]

  expected <- x
  i <- which(names(expected) == "H")
  names(expected)[i] <- "Bar"

  expect_equal(
    rename(x, Bar = H),
    expected)

})

test_that("rename.default() fails if input is not a vector or simple list", {

  x <- matrix(1:9, nrow = 3)
  expect_error(rename(x, Bar = H), "vector or simple list")

})

test_that("rename.default() fails if input has no names", {

  x <- 1:10
  expect_error(rename(x, Bar = H), "must have names")

})
