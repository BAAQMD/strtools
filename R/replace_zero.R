#' Replace zeros with a default value
#'
#' @param x vector containing zeros
#' @param value replacement value
#'
#' @examples
#' x <- c(0, 1, 2, 3)
#' replace_zero(x)
#' replace_zero(x, value = NaN)
#'
#' @export
replace_zero <- function (x, value = NA) {
  replace(x, which(x == 0), value)
}


