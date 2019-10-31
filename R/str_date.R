#' str_date
#'
#' Output a datestamp.
#'
#' @details
#' `str_date()` defaults to `YYYY-mm-dd` (ISO 9601), while `str_datestamp()` defaults to a more concise `YYYYmmdd`. You can override either one.
#'
#' @param format see [format()]
#' @param date defaults to [`Sys.Date()`].
#' @param ... passed to `format()`.
#'
#' @export
str_date <- function (
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
