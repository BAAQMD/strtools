context("str_printable")

test_that("Form Feed", {

  bytes <- c(0x28, 0x12, 0x41)
  chars <- rawToChar(as.raw(bytes))

  expect_equal(chars, "(\022A")
  expect_equal(str_printable(chars), "(A")

})
