context("format_SCC")

test_that("6 digits", {
  expect_identical(format_SCC(8745, digits = 6), "008745")
})

test_that("8 digits", {
  expect_identical(format_SCC(8192373, digits = 8), "08192373")
})
