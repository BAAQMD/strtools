#' Detect strings beginning with some substring
#'
#' @description helpful to combine with `filter()`
#'
#' @param x character vector
#' @param pattern substring that you want to detect
#'
#' @examples
#' x <- c("C22240098", "C22250098", "C22240189", "G1003260")
#' x %>% str_begins("G")
#' x %>% str_begins("C22")
#'
#' library(tidyverse)
#' df <- data_frame(src_code = x)
#' df %>% filter(src_code %>% str_begins("G"))
#' df %>% filter(src_code %>% str_begins("C22"))
#'
#' @export
str_begins <- function (x, pattern) {
  str_detect(x, str_c("^", pattern))
}

