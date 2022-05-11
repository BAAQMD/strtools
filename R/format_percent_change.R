#' format_percent_change
#'
#' Format a relative change as plus/minus percentages (0 means no change).
#'
#' @param x numeric
#' @param na value to use if `x` is `NA`; defaults to an empty string
#' @param zero value to use if `x` is unity; defaults to zero percent (unsigned, no digits).
#'
#' @export
format_percent_change <- function (
  x,
  digits = 0,
  na = "",
  zero = "0%",
  sign = TRUE
) {

  delta <- (x - 1)

  formatted <-
    format_percentage(
      abs(delta),
      digits = digits,
      sign = FALSE)

  if (isTRUE(sign)) {
    formatted <- str_c(str_sign(delta), formatted)
  }

  formatted <-
    if_else(
      delta == 0,
      true = zero,
      false = formatted,
      missing = na)

  return(formatted)

}

