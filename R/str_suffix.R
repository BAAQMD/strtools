#' str_suffix
#'
#' @param x (character) to be suffixed
#' @param suffix (character) length one, or will be recycled
#' @param list (optional) indices of elements to suffix; defaults to all
#'
#' @return (character)
#'
#' @export
#'
#' @examples
#' str_suffix(1:2, "m")
#' str_suffix(1:5, " or more", 3)
#' str_suffix(1:3, c("st", "nd", "rd"))
#'
#' @seealso
#' - [str_prefix()]
str_suffix <- function (x, suffix, list = seq_along(x)) {
  suffixed <- paste0(x, suffix)
  result <- replace(x, list, suffixed[list])
  return(result)
}
