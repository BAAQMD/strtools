#' str_prefix
#'
#' @param x (character) to be prefixed
#' @param suffix (character) length one, or will be recycled
#' @param list (optional) indices of elements to prefix; defaults to all
#'
#' @return (character)
#'
#' @export
#'
#' @examples
#' str_prefix(1:3, "S-")
#' str_prefix(1:5, "<", 1)
#'
#' @seealso
#' - [str_suffix()]
str_prefix <- function (x, prefix, list = seq_along(x)) {
  prefixed <- paste0(prefix, x)
  result <- replace(x, list, prefixed[list])
  return(result)
}
