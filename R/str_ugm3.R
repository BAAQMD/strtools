#' str_ugm3
#'
#' @return
#' @export
#'
str_ugm3 <- function (
  format = c("utf8", "html", "TeX"),
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[str_ugm3] ", ...)

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
    token <- "µg/m³"
  } else if (format == "html") {
    token <- "µg/m<sup>3</sup>"
  } else if (format == "TeX") {
    token <- "µg/m$^3$"
  } else {
    stop("Unknown format ", format)
  }

  return(token)

}
