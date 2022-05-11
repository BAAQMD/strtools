#' format_ugm3
#'
#' @param x numeric
#' @param digits passed to [format_digits()]
#'
#' @return
#' @export
#'
format_ugm3 <- function (
  x,
  digits = 3
) {
  format_digits(drop_units(x), digits = digits)
  x <- set_units(x, "ug/m^3")
}
