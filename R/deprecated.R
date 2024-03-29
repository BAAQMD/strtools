#' glue_TeX
#'
#' @param ... passed to [glue::glue()]
#' @param output passed to [latex2exp::TeX()]
#'
#' @importFrom glue glue
#' @importFrom latex2exp TeX
#'
#' @return suitable for use with ggplot, etc.
#' @export
#'
glue_TeX <- function (..., output = c("expression", "text", "ast")) {
  .Deprecated("str_tex")
  output <- match.arg(output)
  glued <- glue::glue(...)
  return(latex2exp::TeX(glued, output = output))
}
