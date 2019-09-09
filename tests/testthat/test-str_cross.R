context("str_cross")

test_that("str_cross (example)", {

  expect_equal(
    str_cross(c("foo", "bar"), c("baz", "bap"), sep = "-"),
    c("bar-bap", "bar-baz", "foo-bap", "foo-baz"))

})
