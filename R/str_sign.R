#' str_sign
#'
#' Return the signs of signed numbers.
#' Zero, NA, and NaN yield empty strings.
#'
#' @param x numeric
#'
#' @return one of "+", "-", or "".
#'
#' @export
str_sign <- function (x) {
  stopifnot(is.numeric(x))
  x <- as.numeric(x) # in case x is of class "units"
  retval <- rep_along(x, "")
  retval[which(x > 0)] <- "+"
  retval[which(x < 0)] <- "-"
  return(retval)
}
