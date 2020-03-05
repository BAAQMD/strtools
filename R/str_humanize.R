#' Improve readability for humans
#'
#' @param x an integer or numeric vector, or a \code{\link[dplyr]{tbl}}
#' @param digits number of digits to retain
#' @param \dots (optional)
#'
#' @note Per the help for \link{round}, the IEC 60559 ("round to the even digit") is used.
#'       See \code{help(round)} (look under the Details section).
#'
#' @examples
#' x <- c(123456789.01, 0.996, 0.003, 0.007, 0.000001, 0)
#' str_humanize(x)
#' str_humanize(x, digits = 3)
#' str_humanize(x, digits = 3, tiny = "-")
#' str_humanize(x, digits = 2, tiny = "-", zero = "")
#'
#' @export
str_humanize <- function (x, ...) {
  warning("str_humanize() needs more test coverage. Don't use in production!")
  UseMethod("str_humanize")
}

#' @method str_humanize default
#' @noRd
#' @rdname str_humanize
#' @describeIn str_humanize Default: do nothing
#' @export
str_humanize.default <- function (x, ...) {
  return(x)
}

#' @method str_humanize integer
#' @describeIn str_humanize Do nothing to integers
#' @export
str_humanize.integer <- str_humanize.default

#' @param tiny what to print in place of tiny values (that round to zero, given the number of digits you've specified)
#' @param zero what to print in place of actual zeros (that are EXACTLY zero)
#' @param \dots (unused)
#' @method str_humanize numeric
#' @describeIn str_humanize Round real numbers to a given number of digits
#' @importFrom plyr round_any
#' @export
str_humanize.numeric <- function (x, digits = NULL, tiny = NULL, zero = NULL, ...) {

  if (is.null(digits)) {
    #require(scales)
    accuracy <- scales:::precision(x) / (10 ^ getOption("digits"))
    if (accuracy != 0) { # to avoid division by zero in plyr::round_any
      x <- plyr::round_any(x, accuracy)
    }
    digits <- max(0, -1 * log10(accuracy))
  }

  #rounded <- round(x, digits = digits)
  formatted <- formatC(x, digits = digits, format = "f", flag = "#", zero.print = NULL, big.mark = ",")

  is_tiny <- function (x) (x > 0) & (is.finite(x)) & (x < (0.5 * 10^(-digits)))
  is_zero <- function (x) (x == 0)

  if (!is.null(tiny)) {
    formatted <- replace(formatted, is_tiny(x), tiny)
  }

  if (!is.null(zero)) {
    formatted <- replace(formatted, is_zero(x), zero)
  }

  return(formatted)

}

#' @method str_humanize tbl_df
#' @describeIn str_humanize Apply \code{str_humanize(...)} to each column in turn
#' @export
str_humanize.tbl_df <- function (x, ...) {
  f <- function (z) str_humanize(z, ...)
  mutate_all(x, funs(f))
}
