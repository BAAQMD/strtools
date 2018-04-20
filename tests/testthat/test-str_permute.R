context("str_permute")

test_that("LETTERS", {
  ALPHABET <- paste0(LETTERS, collapse = "")
  permuted <- str_permute(rep(ALPHABET, 3), 5:6, 3:4)
  expect_identical(permuted, c("EFCD", "EFCD", "EFCD"))
})
