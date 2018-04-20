context("fixed_width")

expect_equal(
  fixed_width(c(-1, pi, Inf, NA), width = 4, digits = 1),
  c("-1.0", " 3.1", "    ", "    "))

expect_equal(
  fixed_width(as.integer(1:3), width = 2),
  c(" 1", " 2", " 3"))

expect_equal(
  fixed_width(NA_integer_, width = 5),
  "     ")

expect_equal(
  fixed_width("foo", width = 5),
  "foo  ")

expect_equal(
  fixed_width(NA_character_, width = 1),
  " ")
