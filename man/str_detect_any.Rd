% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_detect_any.R
\name{str_detect_any}
\alias{str_detect_any}
\title{str_detect_any}
\usage{
str_detect_any(x, patterns, ignore_case = FALSE, negate = FALSE)
}
\arguments{
\item{x}{(character) Input vector. Either a character vector, or something coercible to one.}

\item{patterns}{(character) One or more patterns to match.}

\item{ignore_case}{(logical) Passed to \code{\link[stringr:str_detect]{stringr::str_detect()}}.}

\item{negate}{(logical) Passed to \code{\link[stringr:str_detect]{stringr::str_detect()}}.}
}
\value{
\code{TRUE} if any element of \code{pattern} is matched in \code{x}; \code{FALSE} otherwise.
}
\description{
str_detect_any
}
