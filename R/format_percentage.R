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
#' @export
format_percentage <- function (
  x,
  digits = 0,
  suffix = "%"
) {

  rounded <-
    round(
      100 * x,
      digits = digits)

  formatted <-
    formatC(
      rounded,
      digits = digits,
      format = "f",
      flag = "#",
      drop0trailing = FALSE) %>%
    str_remove(
      "\\.$")

  suffixed <-
    str_c(
      formatted,
      suffix)

  return(suffixed)

}
