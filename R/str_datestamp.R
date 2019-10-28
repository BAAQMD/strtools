#' str_datestamp
#'
#' Output a datestamp.
#'
#' @param format defaults to `YYYY-mm-dd` (ISO 9601).
#' @param date defaults to [`Sys.Date()`].
#' @param ... passed to `format()`.
#'
#' @export
str_datestamp <- function (
  format = "%Y-%m-%d",
  date = Sys.Date(),
  ...
) {

  datestamp <-
    format(
      date,
      format,
      ...)

  return(datestamp)

}
