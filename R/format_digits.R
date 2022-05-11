#' Format with a fixed number of digits, keeping trailing zeros
#'
#' @description Shorthand for `formatC(x, format="f", flag="#", digits=digits, ...)`.
#'
#' @param x numeric
#' @param digits number of digits to keep; passed to [formatC()]
#' @param na replace `NA` with this value
#' @param ... passed to [formatC()]
#'
#' @export
format_digits <- function (x, digits = NULL, na = NA_character_, ..., sign = FALSE) {
  if (is.null(digits)) {
    digits <- guess_digits(x)
  }
  formatted <- formatC(x, format = "f", flag = "#", digits = digits, ...)
  formatted <- str_remove(formatted, "\\.$")
  if (isTRUE(sign)) {
    i <- which(x > 0)
    formatted[i] <- str_c("+", formatted[i])
  }
  formatted[is.na(x)] <- na
  return(formatted)
}
