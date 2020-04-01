#' Paste values together, separated by commas
#'
#' @param ... (character) strings to concatenate
#' @param collapse (character) separator
#' @param n (integer) concatenate at most this many strings
#' @param na (character) if `na.rm` is `FALSE`, replace `NA` values with this
#' @param na.rm (logical) remove `NA` values? (see also: `na` parameter, above)
#'
#' @importFrom stringr str_c str_replace_na
#'
#' @examples
#' str_csv(1:3)
#' str_csv(1:3, "A", c("B", "C"))
#' str_csv(LETTERS, n = 3)
#'
#' @seealso
#' - [str_and()]
#'
#' @export
str_csv <- function (
  ...,
  collapse = ", ",
  n = Inf,
  na = "NA",
  na.rm = FALSE
) {

  x <- c(...)

  if (n < length(x)) {
    x <- x[1:n]
  }

  if (isTRUE(na.rm)) {
    x <- na.omit(x)
  } else {
    x <- stringr::str_replace_na(
      x,
      replacement = na)
  }

  concatenated <-
    stringr::str_c(
      x,
      collapse = collapse)

  return(concatenated)

}
