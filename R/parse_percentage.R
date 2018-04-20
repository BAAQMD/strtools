#' Parse percentages, dividing by 100
#'
#' @description Converts strings into real numbers. Divides by 100.
#'
#' @note Throws an error if values are out of range. Future versions may offer custom handling.
#'
#' @param x character
#' @param na character
#' @param \dots Further arguments to `readr::parse_double`.
#'
#' @importFrom stringr str_replace_all
#' @importFrom readr parse_double
#'
#' @return numeric
#'
#' @examples
#' parse_percentage("12.0%")
#'
#' @export
parse_percentage <- function (x, na = c("", "NA", "None"), ...) {

  # Might instead use readr::parse_number
  cleaned <- stringr::str_replace_all(x, "%", "")
  parsed <- readr::parse_double(cleaned, na = na, ...)

  decimal_fraction <- parsed / 100.0
  return(decimal_fraction)

}
