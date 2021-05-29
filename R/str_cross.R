#' str_cross
#'
#' Full crossing (all combinations) of given strings
#'
#' @examples
#' str_cross(c("foo", "bar"), c("baz", "bap"), sep = "-")
#'
#' @importFrom tidyr crossing unite
#' @importFrom tibble deframe
#' @importFrom dplyr everything
#' @export
str_cross <- function (..., sep = "") {
  crossed <- crossing(...)
  united <- unite(crossed, .combined, everything(), sep = sep)
  deframe(united)
}
