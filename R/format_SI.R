#' Format according to SI conventions
#'
#' @param x (numeric)
#' @param trim (logical) passed to [format()][base::format()]
#' @param ... further arguments to [format()][base::format()]
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
#' @seealso (Stack Overflow post)[http://stackoverflow.com/questions/21045545/how-to-accurately-display-si-prefix-for-numbers-in-y-axis-scale-of-plot-made-wit]
#'
#' @importFrom stringr str_trim
#' @export
format_SI <- function (
  x,
  signif = 3,
  digits = NULL,
  fixed = FALSE,
  engineering = FALSE,
  trim = TRUE,
  nsmall = 0L,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[format_SI] ", ...)

  log10_breaks <-
    set_names(
      seq(-24, 24, by = 3),
      c("y", "z", "a", "f", "p", "n", "µ", "m", " ", "k", "M", "G", "T", "P", "E", "Z", "Y"))

  if (isTRUE(fixed)) {
    # Single value corresponding to the magnitude of max(x)
    z <- log10(max(abs(x), na.rm = TRUE))
  } else {
    # Vector with array indices according to position in intervals
    z <- log10(abs(x))
  }

  z_adj <- z + 0.5
  msg("max(z) is: ", max(z))
  msg("max(z_adj) is: ", max(z_adj))

  signif_adj <-
    signif

  if (is.null(digits)) {
    digits <- 0
  }

  if (isTRUE(fixed)) {
    digits_adj <- max(digits, 2 + floor(z_adj))
  } else {
    digits_adj <- digits
  }

  msg("digits is: ", digits)
  msg("digits_adj is: ", digits_adj)

  if (isTRUE(engineering)) {
    i <- findInterval(z_adj, log10_breaks)
  } else {
    i <- findInterval(z, log10_breaks)
  }

  # Set prefix to " " for very small values < 1e-24
  i <- if_else(i == 0, which(log10_breaks == 0), i)

  rounded <-
    qtytools::round_half_up(
      x / (10 ^ log10_breaks[i]),
      digits = digits_adj) #round(i / log10(x)))

  # NOTE: to in `format()`, `digits` really means `signif`;
  # `nsmall` means `digits` ... it's confusing!
  formatted <-
    format(
      rounded,
      #trim = trim,
      scientific = FALSE,
      digits = signif,
      nsmall = digits,
      ...)

  str_trim(paste0(formatted, names(log10_breaks)[i]))

}
