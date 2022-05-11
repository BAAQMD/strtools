#' format_ugm3
#'
#' @param x numeric
#' @param digits passed to [format_digits()]
#'
#' @importFrom units drop_units
#'
#' @export
#'
format_ugm3 <- function (
  x,
  digits = 3
) {
  x <- set_units(x, "ug/m^3")
  format_digits(units::drop_units(x), digits = digits)
}
