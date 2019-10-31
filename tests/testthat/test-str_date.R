test_that("str_date() works", {

  # Default case: no arguments
  str_date() %>%
    str_detect("^[0-9]{4}-[0-9]{2}-[0-9]{2}$") %>%
    expect_true()

})

test_that("explicit date format (YYYYmmdd)", {

  # Single argument, unnamed (YYYYmmdd)
  str_date("%Y%m%d") %>%
    str_detect("^[0-9]{4}[0-9]{2}[0-9]{2}$") %>%
    expect_true()

  # Single argument, named (YYYYmmdd)
  str_date(format = "%Y%m%d") %>%
    str_detect("^[0-9]{4}[0-9]{2}[0-9]{2}$") %>%
    expect_true()

})

test_that("explicit date format (YYYY-mm-dd)", {

# Single argument, unnamed (YYYY-mm-dd)
str_date("%Y-%m-%d") %>%
  str_detect("^[0-9]{4}-[0-9]{2}-[0-9]{2}$") %>%
  expect_true()

})

test_that("explicit date (2011-02-03)", {

  test_date <- as.Date("2011-02-03")

  str_date(date = test_date) %>%
    expect_equal("2011-02-03")

  str_date(date = test_date) %>%
    expect_equal("2011-02-03")

})

