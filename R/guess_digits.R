#' guess_digits
#'
#' Really crude heuristic (!) and linear search (!) for an approximate precision.
#'
#' @seealso [format_decimal()]
#'
#' @examples
#' guess_digits(1)
#' guess_digits(0.66)
#' guess_digits(0.667)
guess_digits <- function (x, digits = 0) {

  x <- na.omit(x)
  (d <- round(x, digits) - round(x, digits + 1))

  if (all(d == 0)) {
    return(digits)
  } else {
    (digits <- guess_digits(x, digits + 1))
    return(digits)
  }

}
