#' Format according to SI conventions
#'
#' @param x (numeric)
#' @param ... further arguments to [format][base::format]
#'
#' @examples
#' format_SI(1)
#' format_SI(9:11)
#' format_SI(c(33, 333, 3333, 33333333))
#'
#' @seealso (Stack Overflow post)[http://stackoverflow.com/questions/21045545/how-to-accurately-display-si-prefix-for-numbers-in-y-axis-scale-of-plot-made-wit]
#'
#' @export
format_SI <- function (x, ...) {

  breaks <- 10 ** seq(-24, 24, by = 3)
  prefixes <- c("y", "z", "a", "f", "p", "n", "µ", "m", " ",
                "k", "M", "G", "T", "P", "E", "Z", "Y")

  # Vector with array indices according to position in intervals
  i <- findInterval(abs(x), breaks)

  # Set prefix to " " for very small values < 1e-24
  i <- ifelse(i == 0, which(breaks == 1e0), i)

  rounded <- round(x / breaks[i], 1)
  formatted <- format(rounded, trim = TRUE, scientific = FALSE, ...)

  str_trim(paste0(formatted, prefixes[i]))

}
