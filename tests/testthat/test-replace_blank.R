context("replace_blank")

test_that("simple", {
  x <- c("foo", "", "bar")
  expect_equal(replace_blank(x), c("foo", NA_character_, "bar"))
})

test_that("with NAs", {
  x <- c("foo", "", "bar", NA_character_)
  expect_equal(replace_blank(x), c("foo", NA_character_, "bar", NA_character_))
})
