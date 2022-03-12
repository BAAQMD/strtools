#' format_range
#'
#' @param x (numeric) vector
#' @param digits (numeric) passed to [format_digits()]
#' @param sep (character) separator, like " to " or "-"
#' @param na.rm (logical)
#'
#' @export
#'
#' @examples
#' format_range(1:10)
format_range <- function (x, digits = NULL, sep = " to ", na.rm = TRUE) {
  stopifnot(is.numeric(x))
  f <- function (x) format_digits(x, digits = digits)
  x <- range(x)
  str_glue("{f(min(x, na.rm = na.rm))}{sep}{f(max(x, na.rm = na.rm))}")
}
