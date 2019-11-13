#' format_percent_change
#'
#' Format a relative change as +/-___%.
#'
#' @param x numeric
#' @param na value to use if `x` is `NA`; defaults to an empty string
#' @param zero value to use if `x` is unity; defaults to "0%" (unsigned, no digits).
#'
#' @export
format_percent_change <- function (
  x,
  digits = 0,
  na = "",
  zero = "0%"
) {

  change <- (x - 1)

  formatted <-
    if_else(
      change == 0,
      true = zero,
      false = format_percentage(abs(change), digits = digits),
      missing = na)

  negative <- which(change < 0)
  formatted[negative] <- str_c("-", formatted[negative])

  positive <- which(change > 0)
  formatted[positive] <- str_c("+", formatted[positive])

  return(formatted)

}

