test_that("str_sign() works", {
  expect_equal(str_sign(0), "")
  expect_equal(str_sign(-1.1), "-")
  expect_equal(str_sign(34.5), "+")
  expect_equal(str_sign(Inf), "+")
  expect_equal(str_sign(-Inf), "-")
  expect_equal(str_sign(NaN), "")
  expect_equal(str_sign(NA_real_), "")
  expect_equal(
    str_sign(c(-9.9, 9.6, 2, 0, NA)),
    c("-", "+", "+", "", ""))
})
