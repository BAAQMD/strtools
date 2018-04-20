#' Surround strings with quotation marks
#'
#' @param x character vector
#' @param ch character representing a quotation mark (default is `"`)
#'
#' @examples
#' str_quote(LETTERS)
#'
#' @export
str_quote <- function (x, ch = '"') {
  str_c(ch, x, ch)
}
