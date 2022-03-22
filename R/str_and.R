#' str_and
#'
#' A more human-friendly version of `str_csv()`.
#'
#' @seealso [str_or()]
#'
#' @examples
#' x <- c("apples", "oranges", "pears")
#' str_and(x)
#'
#' @export
str_and <- function (x, ..., .sep = ",") {
  x <- c(x, ...)
  len_x <- length(x)
  if (len_x == 2) {
    return(str_c(x, collapse = " and "))
  } else if (len_x >= 3) {
    x[len_x] <- str_c("and ", x[len_x], collapse = "")
    return(str_c(x, collapse = str_c(.sep, " ")))
  } else {
    return(x)
  }
}
