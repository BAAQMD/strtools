% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_SCC.R
\name{format_SCC}
\alias{format_SCC}
\title{Properly (re)format EPA Source Classification Codes}
\usage{
format_SCC(x, digits)
}
\arguments{
\item{x}{(numeric or character)}

\item{digits}{(integer) almost certainly 6, 8, or 10}
}
\description{
Properly (re)format EPA Source Classification Codes
}
\examples{
format_SCC(8745, digits = 6)
format_SCC(819237, digits = 8)

}
\seealso{
\href{https://ofmpub.epa.gov/sccsearch/}{EPA: Source Classification Codes}
}
