#' Paste values together, separated by commas
#'
#' @param \dots character
#' @importFrom stringr str_c
#'
#' @examples
#' str_csv(1:3)
#' str_csv(1:3, "A", c("B", "C"))
#'
#' @export
str_csv <- function (..., collapse = ", ") {
  x <- c(...)
  stringr::str_c(x, collapse = collapse)
}
