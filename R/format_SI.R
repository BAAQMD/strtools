#' Format according to SI conventions
#'
#' @details
#' If `fixed` is TRUE, the suffix will not be allowed to vary.
#' The suffix corresponding to the largest value in `abs(x)` will be used.
#'
#' If `engineering` is TRUE, the magnitudes will shift upwards by 3
#' for numbers less than 0.
#'
#' @param x (numeric)
#' @param digits (numeric) passed to [format_digits()] under certain conditions (see Details)
#' @param fixed (logical) see Details
#' @param engineering (logical) see Details
#' @param ... reserved for future use / backwards-compatibility
#' @param verbose (logical)
#'
#' @examples
#' format_SI(1)
#' format_SI(9:11)
#' format_SI(c(33, 333, 3333, 33333333))
#' format_SI(seq(0, 1.2, len = 7) * 1000)
#' format_SI(seq(0, 1.2, length.out = 5) * 1e3, fixed = TRUE)
#' format_SI(seq(0, 0.9, length.out = 4) * 1e3, fixed = TRUE)
#'
#' format_SI(seq(0, 0.9, length.out = 4) * 1e3, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.9, length.out = 4) * 1e4, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.9, length.out = 4) * 1e5, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.9, length.out = 4) * 1e6, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.9, length.out = 4) * 1e7, fixed = TRUE, engineering = TRUE)
#'
#' format_SI(seq(0, 1.25, length.out = 6) * 1e2, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 1.25, length.out = 6) * 1e3, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 1.25, length.out = 6) * 1e4, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 1.25, length.out = 6) * 1e5, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 1.25, length.out = 6) * 1e6, fixed = TRUE, engineering = TRUE)
#'
#' format_SI(seq(0, 0.75, length.out = 4) * 1e0, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.75, length.out = 4) * 1e1, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.75, length.out = 4) * 1e2, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.75, length.out = 4) * 1e3, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.75, length.out = 4) * 1e4, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.75, length.out = 4) * 1e5, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.75, length.out = 4) * 1e6, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.75, length.out = 4) * 1e7, fixed = TRUE, engineering = TRUE)
#' format_SI(seq(0, 0.75, length.out = 4) * 1e8, fixed = TRUE, engineering = TRUE)
#'
#' format_SI(seq(0, 1.00, length.out = 5) * 1e8)
#'
#' @seealso
#' - [format_qty()]
#' - [format_digits()]
#'
#' @importFrom stringr str_trim
#' @export
format_SI <- function (
  x,
  digits = NULL,
  fixed = FALSE,
  engineering = FALSE,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[format_SI] ", ...)

  LOG10_BREAKS <-
    set_names(
      seq(-24, 24, by = 3),
      c("y", "z", "a", "f", "p", "n", "µ", "m", " ", "k", "M", "G", "T", "P", "E", "Z", "Y"))

  z <- log10(abs(x))

  if (isTRUE(engineering)) {
    (z <- if_else(z < 0, z + 3, z))
    z <- z[which.max(z)]
  } else if (isTRUE(fixed)) {
    z <- z[which.max(z)]
  }

  # Look up the suffix corresponding to z.
  # For very small values (less than 1e-24), use the suffix for zero (""),
  # since no suffix for those is defined above.
  i <- findInterval(z, LOG10_BREAKS)
  i <- replace(i, is.infinite(z), which(LOG10_BREAKS == 0))
  suffix <- names(LOG10_BREAKS)[i]

  # Divide out the magnitude, and round to the appropriate number of digits.
  magnitude <- 10 ^ LOG10_BREAKS[i]
  value <- x / magnitude

  if (is.null(digits) && isFALSE(engineering)) {
    formatted <- format_digits(value, digits = 0)
  } else {
    stopifnot(is.numeric(digits))
    formatted <- format_digits(value, digits = digits)
  }

  (suffixed <- str_trim(str_c(formatted, suffix)))
  return(suffixed)

}
