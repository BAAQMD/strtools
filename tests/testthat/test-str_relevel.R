context("str_relevel")

test_that("LETTERS", {

  expect_equal(
    str_relevel(LETTERS[1:10], "E"),
    c("E", "A", "B", "C", "D", "F", "G", "H", "I", "J"))

})
