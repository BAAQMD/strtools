#' format_qty
#'
#' Shortcut for `format_SI(..., fixed = TRUE, engineering = TRUE)`.
#'
#' @usage format_qty(...)
#'
#' @export
format_qty <- function (
  ...,
  fixed = TRUE,
  engineering = TRUE,
  verbose = getOption("verbose")
) {

  formatted <-
    format_SI(
      ...,
      fixed = fixed,
      engineering = engineering)

  return(formatted)

}
