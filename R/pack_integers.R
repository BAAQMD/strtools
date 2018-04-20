#' Packed representation of integers (R notation).
#'
#' @param x vector of integers (integer-ish is OK)
#'
#' @return string
#' @export
#'
#' @examples
#' x <- c(1, 3, 4:7, 9)
#' pack_integers(x)
pack_integers <- function (x) {

  x <- sort(unique(x)) # put elements of x in non-decreasing order
  edges <- which(diff(x) > 1) # find "edges" amongst non-decreasing integers

  range_ends <- union(edges, length(x))
  range_beginnings <- union(1, edges + 1)

  pack <- function (a, b) if (a == b) a else paste(a, b, sep = ":")
  packed <- mapply(pack, x[range_beginnings], x[range_ends])
  collapsed <- paste(packed, collapse = ", ")

  paste0("c(", collapsed, ")")

}
