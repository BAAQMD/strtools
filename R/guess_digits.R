#' guess_digits
#'
#' @seealso
#' - [format_digits()]
#' - [format_SI()]
#' - [format_qty()]
#'
#' @examples
#' guess_digits(c(1, 0.6, 0.25))
#' guess_digits(0.66)
#' guess_digits(0.667)
#'
guess_digits <- function (x, max = base::max(c(6, getOption("digits")))) {

  stopifnot(is.numeric(x))

  # Note: formatC() will complain if digits > 50 here; see ?formatC
  formatted <- formatC(x, digits = max, drop0trailing = TRUE)

  # Instances of a decimal point w/ trailing digits
  trailing <- na.omit(str_match(formatted, "\\.[0-9]+"))

  if (length(trailing) == 0) {
    digits <- 0
  } else {
    # Don't count the decimal point
    digits <- max(nchar(trailing)) - 1
  }

  return(digits)

}
