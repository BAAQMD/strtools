#' rename.default
#'
#' @param x named vector or simple named list
#' @param ... further arguments, as with [dplyr::rename()]
#'
#' @importFrom dplyr rename
#' @importFrom rlang set_names
#'
#' @export
rename.default <- function (x, ...) {
  if (isFALSE(is.vector(x))) {
    stop("input must be named vector or simple list")
  }
  if (is.null(names(x))) {
    stop("input vector or list must have names")
  }
  dat <- do.call(data.frame, as.list(x))
  renamed <- dplyr::rename(dat, ...)
  rlang::set_names(x, names(renamed))
}
