#' format_qty
#'
#' Shortcut for `format_SI(..., fixed = TRUE, engineering = TRUE)`.
#'
#' @usage format_qty(...)
#'
#' @export
format_qty <- function (
  x,
  digits = NULL,
  # fixed = TRUE,
  engineering = TRUE,
  ...,
  verbose = getOption("verbose")
) {

  if (is.null(digits)) {
    fixed <- TRUE
  } else {
    fixed <- FALSE
  }

  formatted <-
    format_SI(
      x,
      digits = digits,
      fixed = fixed,
      engineering = engineering,
      ...,
      verbose = verbose)

  return(formatted)

}
