#' format_range
#'
#' @param x (numeric) vector
#' @param digits (numeric) passed to [format_digits()]
#' @param sep (character) separator, like " to " or "–"
#' @param na.rm (logical)
#'
#' @export
#'
#' @examples
#' format_range(1:10)
format_range <- function (
    x,
    digits = NULL,
    sep = "–",
    na = "NA",
    na.rm = FALSE
) {

  stopifnot(is.numeric(x))

  lower <- min(x, na.rm = na.rm)
  upper <- max(x, na.rm = na.rm)

  if (is.na(lower) || is.na(upper)) {
    stop("[format_range] NA detected")
  }

  fmt <- function (x) format_digits(x, digits = digits)

  formatted <- str_c(
    fmt(lower),
    sep,
    fmt(upper))

  return(formatted)

}
