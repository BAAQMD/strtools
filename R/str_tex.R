#' str_tex
#'
#' Glues its `...` argument(s) together, then runs the result through [latex2exp::TeX()].
#'
#' @param ... (character) combined via [stringr::str_glue(...)].
#'
#' @importFrom stringr str_glue
#' @importFrom latex2exp TeX
#'
#' @return
#' @export
#'
#' @examples
str_tex <- function (...) {
  glued <- stringr::str_glue(...)
  tex <- latex2exp::TeX(as.character(glued))
  return(tex)
}
