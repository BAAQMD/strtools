#' str_datetime
#'
#' Returns the current date and time, as a formatted string.
#'
#' @param format (character) see [strftime()]
#'
#' @return (character)
#'
#' @export
str_datetime <- function (format = "%Y%m%d-%H%M%S") {
  base::format(Sys.time(), format)
}
