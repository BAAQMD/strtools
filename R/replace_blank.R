#' Replace blank character strings with a default value
#'
#' @param x vector containing ""s
#' @param value replacement value
#'
#' @examples
#' x <- c("foo", "", "bar")
#' replace_blank(x)
#' replace_blank(x, value = "-8888")
#'
#' @export
replace_blank <- function (x, value = NA) {
  replace(x, which(x == ""), value)
}
