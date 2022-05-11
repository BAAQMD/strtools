#' Format percentages
#'
#' Express fractions as percentages, to the precision given by `digits`.
#'
#' @param x (numeric)
#' @return (character)
#'
#' @examples
#' format_percentage(1.00)
#' format_percentage(2/3)
#' format_percentage(2/3, digits = 3)
#'
#' @importFrom stringr str_remove
#' @export
format_percentage <- function (
  x,
  digits = 0,
  suffix = "%",
  ...
) {
  formatted <- format_digits(100 * x, digits = digits, ...)
  suffixed <- str_c(formatted, suffix)
  return(suffixed)
}
