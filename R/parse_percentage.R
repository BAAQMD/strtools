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

  parse_value <- function (x) {
    parsed <- readr::parse_double(x, na = union(na, c("Inf", "-Inf")), ...)
    parsed[x == "Inf"] <- Inf
    parsed[x == "-Inf"] <- -Inf
    return(parsed)
  }

  stripped <- stringr::str_remove(x, "%$")
  fraction <- parse_value(stripped) / 100.0

  return(fraction)

}
