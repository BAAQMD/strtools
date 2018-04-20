#' precision
#'
#' @param x numeric vector
#'
#' @return a power of ten (10, 100, 1000, etc.)
#' @note Lifted from \pkg{scales} (doesn't export \code{precision})
#'
#' @importFrom scales zero_range
precision <- function (x) {
  rng <- range(x, na.rm = TRUE)
  span <- if (scales::zero_range(rng))
    rng[1]
  else diff(rng)
  10 ^ floor(log10(span))
}
