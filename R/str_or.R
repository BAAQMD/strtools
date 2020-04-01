#' str_or
#'
#' A more human-friendly version of `str_csv()`.
#'
#' @seealso [str_and()]
#'
#' @examples
#' x <- c("apples", "oranges", "pears")
#' str_or(x)
#'
#' @export
str_or <- function (x, ...) {
  x <- c(x, ...)
  len_x <- length(x)
  if (len_x == 2) {
    return(str_c(x, collapse = " or "))
  } else if (len_x >= 3) {
    x[len_x] <- str_c("or ", x[len_x], collapse = "")
    return(str_c(x, collapse = ", "))
  } else {
    return(x)
  }
}
