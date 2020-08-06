#' format_count
#'
#' Format counts, using commas as needed.
#' Defaults to 0 decimal places.
#'
#' @param x numerix
#' @param digits integer
#' @param trim trim off excess whitespace (defalut `TRUE`)
#' @param ... passed to [formatC()]
#'
#' @return character
#'
#' @export
format_count <- function (x, digits = 0, trim = TRUE, ...) {

  try(
    x <- units::drop_units(x),
    silent = TRUE)

  formatted <-
    formatC(
      x,
      big.mark = ",",
      flag = "#",
      format = "f",
      digits = digits,
      drop0trailing = FALSE,
      ...)

  if (isTRUE(trim)) {
    formatted <- str_trim(formatted)
  }

  return(formatted)

}
