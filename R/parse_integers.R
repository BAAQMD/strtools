#' Parse a single character string representing a vector of integers
#'
#' @seealso unpack_ids
#'
#' @note TODO: ensure no rounding occurs (validate input; no decimal places)?
#'
#' @examples
#' parse_integers("1")
#' parse_integers("c(2, 3)")
#' parse_integers("c(5:9)")
#' parse_integers("5:9")
#' parse_integers("NA")
#' parse_integers("foo")
#'
#' @export
parse_integers <- function (x) {

  if (is.numeric(x)) {
    return(as.integer(x))
  } else {
    x <- if_else(is.na(x), "NA", as.character(x))
  }

  patterns <- c("^c\\([0-9:, ]+\\)$", "^[0-9]+$", "^[0-9]+:[0-9]+$", "^NA$", "^$")
  detections <- map(patterns, partial(str_detect, string = x))
  valid <- reduce(detections, `|`, .init = FALSE)

  if (!all(valid)) {
    stop(str_c("Couldn't parse the following: \"", x[!valid], "\""))
  }

  sandbox <- new.env()
  safe_eval <- function (x) eval(parse(text = x), envir = sandbox)
  evaluated <- map(x, safe_eval)
  return(map(evaluated, as.integer))

}
