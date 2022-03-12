#' str_tex
#'
#' Glues its `...` argument(s) together, then runs the result through [latex2exp::TeX()].
#'
#' @param ... (character) combined via [stringr::str_glue(...)].
#' @param output (character) one of "expression", "text", or "ast"; see [latex2exp::TeX()].
#'
#' @importFrom stringr str_glue
#' @importFrom latex2exp TeX
#'
#' @return
#' @export
#'
#' @examples
str_tex <- function (..., output = c("expression", "text", "ast")) {
  output <- match.arg(output)
  glued <- stringr::str_glue(...)
  tex <- latex2exp::TeX(as.character(glued), output = output)
  return(tex)
}
