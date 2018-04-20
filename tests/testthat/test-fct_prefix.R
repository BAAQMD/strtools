context("fct_prefix")

test_that("integers", {

  x <- c(33, 156, 44)
  prefixed <- fct_prefix(x, prefix = "#")

  expect_identical(
    prefixed,
    structure(c(1L, 3L, 2L), .Label = c("#33", "#44", "#156"), class = "factor"))

  expect_equal(
    as.character(sort(prefixed)),
    c("#33", "#44", "#156"))

})
