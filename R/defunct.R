#
# Put your defunct code here, for easier housekeeping.
#
# Prune this every so often.
#

#' @export
sci_format <- function (x, ...) {
  .Defunct(msg = "use strtools::format_scientific() instead")
}

#' @export
str_titlecase <- function (x) {
  .Defunct(msg = "use stringr::str_to_title() instead")
}

#' @export
str_lowercase <- function (x) {
  .Defunct(msg = "use stringr::str_to_lower() instead")
}

#' @export
str_uppercase <- function (x) {
  .Defunct(msg = "use stringr::str_to_upper() instead")
}

#' Element-wise replacement for vectors
#'
#' @noRd
#'
#' @name replace_which
#'
#' @param x   original vector
#' @param i   elements to replace (numeric or logical)
#' @param r   alternative value(s)
#'
#' @note \code{r} can either be of length 1 (in which case this behaves like a faster \code{ifelse}) or the same length as \code{x} (in which case this can serve as a more readable alternative to \code{replace}).
#'
#' @return a vector of the same length as \code{x}, with elements \code{x[i]} replaced by \code{r[i]}
#'
#' @examples
#' library(tidyverse)
#' x <- c(1, 2, NA, 4, 5)
#' x %>% replace_which(is.na(.), -8888)
#' x %>% replace_which(. > 3, -8888)
#'
#' @export
replace_which <- function (x, i, r) {

  .Defunct("base::replace()")

  if (length(r) == length(x)) {
    new_values <- r[i]
  } else if (length(r) == 1) {
    new_values <- r
  } else {
    stop("length(r) is not 1 or length(x)")
  }
  replace(x, i, new_values)
}

#' Replace NAs with a default value
#'
#' @noRd
#'
#' @name replace_NA
#'
#' @param x vector containing zeros
#' @param value replacement value
#'
#' @examples
#' x <- c(1, 2, NA)
#' replace_NA(x)
#' replace_NA(x, value = -8888)
#'
#' @export
replace_NA <- function (x, value = 0) {
  .Defunct("Use replace_na() instead")
  replace(x, which(is.na(x)), value)
}

#' Format numbers, rounding to a given number of digits
#'
#' @param input_data tbl containing columns to total
#' @param digits number of digits
#' @param \dots additional parameters passed on to methods
#' @export
format_each <- function (input_data, ..., digits) {

  .Defunct("mutate_at(vars(...), funs(round), digits = digits)")

  # f <- function (x) round(x, digits = digits)
  # # WAS: mutate_each_(input_data, funs(f), lazyeval::lazy_dots(...))
  # mutate_at(input_data, vars(...), funs(f))

}

#' @export
dput_ranges <- function (x) {

  .Deprecated(
    "dput_ranges",
    msg = "`dput_ranges()` is deprecated. Please consider modifying your code to use `pack_integers()` instead.")

  x <- sort(unique(x))

  edges <- which(diff(x) > 1)
  ends <- union(edges, length(x))
  starts <- union(1, edges + 1)

  paste_range <- function (a, b) if (a == b) a else paste(a, b, sep = ":")

  mapply(paste_range, x[starts], x[ends]) %>% paste(collapse = ", ")

}

#' Scientific format (Unicode)
#'
#' @examples
#' format_scientific(1.23e-5, digits = 3)
#' format_scientific(11, digits = 2)
#' format_scientific(9.0, digits = 2)
#'
#' @export
format_scientific <- function (x, ...) {
  .Defunct()
  UseMethod("format_scientific")
}

#' @export
format_scientific.default <- function (x, digits = getOption("digits"), ...) {
  formatC(x, format = "e", digits = digits, ...) %>% sci_format
}

#' @importFrom stringr str_match_all
#' @export
format_scientific.character <- function (x, ...) {
  SUPERSCRIPT_NUMERALS <- c(
    "\U2070", "\U00B9", "\U00B2", "\U00B3",
    "\U2074", "\U2075", "\U2076", "\U2077", "\U2078", "\U2079")
  parts <- str_match_all(x, "([^e]+)e([+-])([0-9]+)")
  num <- sapply(parts, function (x) x[[2]])
  exp_sgn <- sapply(parts, function (x) ifelse(x[[3]] == "-", "\U207B", ""))
  exp_num <- sapply(parts, function (x) SUPERSCRIPT_NUMERALS[[as.integer(x[[4]]) + 1]])
  paste(num, "\U00D7", "10", exp_sgn, exp_num, sep = "")
}

