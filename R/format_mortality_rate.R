#' format_mortality_rate
#'
#' @param x value
#' @param units to express `value` in
#' @param digits passed to [format_count()]
#' @param signif passed to [base::signif()]
#' @param suffix (optional) will default to "/M" if units are "/Mperson"
#'
#' @importFrom stringr str_detect str_c
#' @importFrom units set_units
#'
#' @export
#'
format_mortality_rate <- function (
  x,
  units = "death/Mperson",
  digits = NULL,
  signif = Inf,
  suffix = NULL
) {

  if (is.null(digits)) {
    if (units == "death/person") {
      digits <- 5
    } else if (units == "death/Mperson") {
      digits <- 0
    } else {
      stop("Please supply `digits = ...` to `format_mortality_rate()`.")
    }
  }

  converted <-
    units::set_units(x, stringr::str_c(units, "/yr"), mode = "character")

  naive <-
    drop_units(converted)

  approximated <-
    base::signif(
      naive,
      digits = signif)

  formatted <-
    format_count(
      approximated,
      digits = digits)

  if (is.null(suffix)) {
    if (stringr::str_detect(units, "Mperson")) {
      suffix = "/M"
    } else {
      suffix = ""
    }
  }

  formatted <-
    stringr::str_c(
      formatted,
      suffix)

  return(formatted)

}
