#' str_printable
#'
#' @examples
#' x <- as.raw(c(0x28, 0x12, 0x41))
#' print(rawToChar(x))
#' str_printable(rawToChar(x))
#'
#' @export
str_printable <- function (x, non_printable = "[^[:print:]]") {
  str_remove_all(x, non_printable)
}

