#' str_PM25
#'
#' @return
#' @export
#'
str_PM25 <- function (
  format = c("utf8", "html", "TeX"),
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

  if (format == "utf8") {
    token <- "PM2.5"
  } else if (format == "html") {
    token <- "PM<sub>2.5</sub>"
  } else if (format == "TeX") {
    token <- "PM$_{2.5}$"
  } else {
    stop("Unknown format ", format)
  }

  return(token)

}
