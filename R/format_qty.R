#' @export
format_qty <- function (x, unit = NULL, format = NULL, digits = 1, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[format_qty] ", ...)

  formatter <- format
  format <- base::format

  if (is.null(formatter)) {

    msg("using default formatter (adaptation of format_SI)")

    formatter <- function (x, ...) {

      breaks <- 10 ** seq(-24, 24, by = 3)
      prefixes <- c("y", "z", "a", "f", "p", "n", "µ", "m", "",
                    "k", "M", "G", "T", "P", "E", "Z", "Y")

      # Vector with array indices according to position in intervals
      i <- findInterval(abs(x), breaks)

      #if (diff(range(i, na.rm = TRUE)) > 3) {
      #  warn_msg <- str_c("[format_qty] values span > 3 orders of magnitude; consider using `format_SI`")
      #  warning(warn_msg)
      #}

      max_i <- max(i, na.rm = TRUE)
      max_prefix <- prefixes[max_i]
      rounded <- round(x / breaks[max_i], digits = digits)
      formatted <- str_trim(base::format(rounded, trim = TRUE, scientific = FALSE, ...))
      paste0(formatted, max_prefix)

    }

  }

  formatted <- formatter(x, ...)

  if (!is.null(unit)) {
    i <- which(!is.na(x))
    formatted[last(i)] <- str_c("\n", last(formatted[i]), "\n", unit)
  }

  return(formatted)

}
