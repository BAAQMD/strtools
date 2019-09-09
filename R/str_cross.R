#' str_cross
#'
#' Full crossing (all combinations) of given strings
#'
#' @examples
#' str_cross(c("foo", "bar"), c("baz", "bap"), sep = "-")
#'
#' @export
str_cross <- function (..., sep = "") {
  crossed <- crossing(...)
  united <- unite(crossed, .combined, everything(), sep = sep)
  deframe(united)
}
