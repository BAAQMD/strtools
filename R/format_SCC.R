#' Properly (re)format EPA Source Classification Codes
#'
#' @param x (numeric or character)
#' @param digits (integer) almost certainly 6, 8, or 10
#'
#' @examples
#' format_SCC(8745, digits = 6)
#' format_SCC(819237, digits = 8)
#'
#' @seealso [EPA: Source Classification Codes](https://ofmpub.epa.gov/sccsearch/)
#'
#' @export
format_SCC <- function (x, digits) {
  stopifnot(digits %in% c(6, 8, 10))
  extracted <- stringr::str_sub(x, 1, digits)
  parsed <- readr::parse_number(extracted)
  padded <- stringr::str_pad(parsed, width = digits, side = "left", pad = "0")
  return(padded)
}
