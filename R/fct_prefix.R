#' Prefix something whilst preserving original sort order
#'
#' @param x numeric, string, etc.
#' @param prefix string
#'
#' @return a factor that sorts in the same order as x
#'
#' @examples
#' x <- c(33, 156, 44)
#' prefixed <- fct_prefix(x, prefix = "#")
#' sort(prefixed)
#' sort(str_c("#", x)) # not the same
#'
#' @note FIXME: move to more appropriate home (package)
#'
#' @export
fct_prefix <- function (x, prefix) {
  # TODO: handle NAs
  lvls <- sort(unique(x))
  factor(x, levels = lvls, labels = str_c(prefix, lvls))
}
