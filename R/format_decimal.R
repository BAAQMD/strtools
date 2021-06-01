#' format_decimal
#'
#' @importFrom stringr fixed
#' @export
format_decimal <- function (x, ..., signif = Inf) {

  if (is.finite(signif)) {
    rounded <- base::signif(x, digits = signif)
    formatted <- formatC(rounded, format = "E")
  } else {
    formatted <- formatC(x, format = "f", digits = guess_digits(x))
  }

  tidied <-
    if_else(str_detect(formatted, fixed("NA")),
            NA_character_,
            formatted)

  class(tidied) <- c(class(tidied), "decimal")
  return(tidied)

}
