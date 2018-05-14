#' Relevel a character vector
#'
#' Move the first occurrence of a value to the first position.
#'
#' @param x (character) vector
#' @param what (character) value to look for
#'
#' @examples
#' str_relevel(LETTERS, "J")
#' str_relevel(LETTERS, "foo")
#'
#'  @export
str_relevel <- function (x, what) {
  i <- first(match(what, x))
  if (is.na(i)) stop(str_c("no elements are equal to '", what, "'"))
  c(x[i], x[-i])
}
