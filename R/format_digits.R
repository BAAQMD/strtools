#' Format with a fixed number of digits, keeping trailing zeros
#'
#' @description Shorthand for `formatC(x, format="f", flag="#", digits=digits, ...)`.
#'
#' @param x numeric
#' @param digits number of digits to keep
#' @param ... passed to [formatC()]
#'
#' @export
format_digits <- function (x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- guess_digits(x)
  }
  formatted <- formatC(x, format = "f", flag = "#", digits = digits, ...)
  formatted <- str_remove(formatted, "\\.$")
  return(formatted)
}
