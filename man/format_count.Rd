% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_count.R
\name{format_count}
\alias{format_count}
\title{format_count}
\usage{
format_count(x, digits = 0, trim = TRUE, ...)
}
\arguments{
\item{x}{numerix}

\item{digits}{integer}

\item{trim}{trim off excess whitespace (defalut \code{TRUE})}

\item{...}{passed to \code{\link[=formatC]{formatC()}}}
}
\value{
character
}
\description{
Format counts, using commas as needed.
Defaults to 0 decimal places.
}
