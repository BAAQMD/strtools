#' str_datestamp
#'
#' @rdname str_date
#'
#' @export
str_datestamp <- function (
  format = "%Y%m%d",
  date = Sys.Date(),
  ...
) {

  str_date(
    format = format,
    date = date,
    ...)

}
