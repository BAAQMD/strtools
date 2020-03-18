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
str_csv <- function (..., collapse = ", ", na.rm = FALSE) {

  x <- c(...)

  if (isTRUE(na.rm)) {
    x <- na.omit(...)
  } else {
    x <- stringr::str_replace_na(
      x,
      replacement = "NA")
  }

  concatenated <-
    stringr::str_c(
      x,
      collapse = collapse)

  return(concatenated)

}
