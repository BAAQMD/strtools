#' str_NOx
#'
#' @param format (character) one of "character", "utf8", "html", "TeX", or "markdown"
#' @param verbose (logical)
#'
#' @return representation of "NOx" (nitrogen oxides)
#'
#' @export
str_NOx <- function (
  format = c("character", "utf8", "html", "TeX", "markdown"),
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[str_PM25] ", ...)

  if (missing(format)) {
    if (isTRUE(knitr::is_html_output())) {
      format <- "html"
    } else if (isTRUE(knitr::is_latex_output())) {
      format <- "TeX"
    } else {
      format <- match.arg(format)
    }
  } else {
    format <- match.arg(format)
  }

  if (format %in% c("character", "utf8")) {
    token <- "NOx"
  } else if (format == "html") {
    token <- "NO<sub>x</sub>"
  } else if (format == "TeX") {
    token <- "NO_x"
  } else if (format == "markdown") {
    token <- "NO~x~"
  } else {
    stop("Unknown format ", format)
  }

  return(token)

}
