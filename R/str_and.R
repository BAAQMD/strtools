#' str_and
#'
#' A more human-friendly version of `str_csv()`.
#'
#' @examples
#' x <- c("apples", "oranges", "pears")
#' str_and(x)
#'
#' @export
str_and <- function (x, ...) {
  x <- c(x, ...)
  len_x <- length(x)
  if (len_x == 2) {
    return(str_c(x, collapse = " and "))
  } else if (len_x >= 3) {
    x[len_x] <- str_c("and ", x[len_x], collapse = "")
    return(str_c(x, collapse = ", "))
  } else {
    return(x)
  }
}
