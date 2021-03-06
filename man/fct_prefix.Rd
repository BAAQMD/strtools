% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_prefix.R
\name{fct_prefix}
\alias{fct_prefix}
\title{Prefix something whilst preserving original sort order}
\usage{
fct_prefix(x, prefix)
}
\arguments{
\item{x}{numeric, string, etc.}

\item{prefix}{string}
}
\value{
a factor that sorts in the same order as x
}
\description{
Prefix something whilst preserving original sort order
}
\note{
FIXME: move to more appropriate home (package)
}
\examples{
x <- c(33, 156, 44)
prefixed <- fct_prefix(x, prefix = "#")
sort(prefixed)
sort(str_c("#", x)) # not the same

}
