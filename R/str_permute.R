#' Extract and collapse substrings
#'
#' @examples
#' ALPHABET <- paste0(LETTERS, collapse = "")
#' str_permute(rep(ALPHABET, 3), 5:6, 3:4)
#'
#' @export
str_permute <- function (x, ...) {

  make_extractor <- function (i) {
    rng <- range(i)
    function (s) do.call(str_sub, append(list(s), as.list(rng)))
  }

  extractors <- lapply(list(...), make_extractor)
  permuted_substrings <- sapply(extractors, function (f) f(x))
  apply(permuted_substrings, MARGIN = 1, FUN = str_c, collapse = "")

}
