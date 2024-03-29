% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_tex.R
\name{str_tex}
\alias{str_tex}
\title{str_tex}
\usage{
str_tex(..., output = c("expression", "text", "ast"))
}
\arguments{
\item{...}{(character) combined via \code{\link[stringr:str_glue]{stringr::str_glue()}}.}

\item{output}{(character) one of "expression", "text", or "ast"; see \code{\link[latex2exp:TeX]{latex2exp::TeX()}}.}
}
\description{
Glues its \code{...} argument(s) together, then runs the result through \code{\link[latex2exp:TeX]{latex2exp::TeX()}}.
}
\examples{
str_tex("$exp(x^\\\\beta))$")

}
