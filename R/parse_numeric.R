#' parse_numeric
#'
#' @param x character
#' @return numeric
#'
#' @export
parse_numeric <- function (x) {

  # extract all digits and decimal points
  pattern <- "[[:digit:].,+-]+(\\s*[Ee][+-]?[0-9]+)?"
  extracted <- stringr::str_extract(x, pattern)

  # FIXME: check for more than one '.' here

  # remove all commas
  cleaned <- stringr::str_remove_all(extracted, "[,\\s]")

  # parse as double
  parsed <- readr::parse_double(cleaned)

  return(parsed)

}
