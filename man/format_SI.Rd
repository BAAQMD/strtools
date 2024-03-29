% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_SI.R
\name{format_SI}
\alias{format_SI}
\title{Format according to SI conventions}
\usage{
format_SI(
  x,
  digits = NULL,
  fixed = FALSE,
  engineering = FALSE,
  ...,
  verbose = getOption("verbose")
)
}
\arguments{
\item{x}{(numeric)}

\item{digits}{(numeric) passed to \code{\link[=format_digits]{format_digits()}} under certain conditions (see Details)}

\item{fixed}{(logical) see Details}

\item{engineering}{(logical) see Details}

\item{...}{reserved for future use / backwards-compatibility}

\item{verbose}{(logical)}
}
\description{
Format according to SI conventions
}
\details{
If \code{fixed} is TRUE, the suffix will not be allowed to vary.
The suffix corresponding to the largest value in \code{abs(x)} will be used.

If \code{engineering} is TRUE, the magnitudes will shift upwards by 3
for numbers less than 0.
}
\examples{
format_SI(1)
format_SI(9:11)
format_SI(c(33, 333, 3333, 33333333))
format_SI(seq(0, 1.2, len = 7) * 1000)
format_SI(seq(0, 1.2, length.out = 5) * 1e3, fixed = TRUE)
format_SI(seq(0, 0.9, length.out = 4) * 1e3, fixed = TRUE)

format_SI(seq(0, 0.9, length.out = 4) * 1e3, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.9, length.out = 4) * 1e4, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.9, length.out = 4) * 1e5, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.9, length.out = 4) * 1e6, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.9, length.out = 4) * 1e7, fixed = TRUE, engineering = TRUE)

format_SI(seq(0, 1.25, length.out = 6) * 1e2, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 1.25, length.out = 6) * 1e3, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 1.25, length.out = 6) * 1e4, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 1.25, length.out = 6) * 1e5, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 1.25, length.out = 6) * 1e6, fixed = TRUE, engineering = TRUE)

format_SI(seq(0, 0.75, length.out = 4) * 1e0, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.75, length.out = 4) * 1e1, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.75, length.out = 4) * 1e2, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.75, length.out = 4) * 1e3, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.75, length.out = 4) * 1e4, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.75, length.out = 4) * 1e5, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.75, length.out = 4) * 1e6, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.75, length.out = 4) * 1e7, fixed = TRUE, engineering = TRUE)
format_SI(seq(0, 0.75, length.out = 4) * 1e8, fixed = TRUE, engineering = TRUE)

format_SI(seq(0, 1.00, length.out = 5) * 1e8)

}
\seealso{
\itemize{
\item \code{\link[=format_qty]{format_qty()}}
\item \code{\link[=format_digits]{format_digits()}}
}
}
