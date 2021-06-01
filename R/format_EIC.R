#' format_EIC
#'
#' @note FIXME: make this available to `../data-raw/SIP2016_crosswalk.R`
#'
#' @importFrom stringr str_pad str_sub str_detect
#' @export
format_EIC <- function (x) {
  is_only_digits <- function (...) str_detect(..., "^[[:digit:]]+$")
  d <- if_else(is_only_digits(x), str_pad(x, 14, "left", "0"), NA_character_)
  str_c(str_sub(d, 1, 3), str_sub(d, 4, 6), str_sub(d, 7, 10), str_sub(d, 11, 14), sep = "-")
}

