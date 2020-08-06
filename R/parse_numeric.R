#' parse_numeric
#'
#' @param x character
#' @return numeric
#'
#' @export
parse_numeric <- function (x) {

  # extract all digits and decimal points
  extracted <- stringr::str_extract(x, "[[:digit:].,+-]+")

  # FIXME: check for more than one '.' here

  # remove all commas
  cleaned <- stringr::str_remove_all(extracted, ",")

  # parse as double
  parsed <- readr::parse_double(cleaned)

  return(parsed)

}
