#' str_detect_any
#'
#' @param x (character) Input vector. Either a character vector, or something coercible to one.
#' @param patterns (character) One or more patterns to match.
#' @param ignore_case (logical) Passed to [stringr::str_detect()].
#' @param negate (logical) Passed to [stringr::str_detect()].
#'
#' @return `TRUE` if any element of `pattern` is matched in `x`; `FALSE` otherwise.
#'
#' @export
str_detect_any <- function (
  x,
  patterns,
  ignore_case = FALSE,
  negate = FALSE
) {

  f <- function (pattern) {
    str_detect(x, regex(pattern, ignore_case = ignore_case), negate = negate)
  }

  match_list <- map(patterns, f)
  result <- reduce(match_list, `|`)

  return(result)

}
