% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_draft.R
\name{str_draft}
\alias{str_draft}
\title{str_draft}
\usage{
str_draft(...)
}
\arguments{
\item{...}{(optional) appended with \code{\link[stringr:str_c]{stringr::str_c()}}.}
}
\description{
Outputs "DRAFT" and the current date. #'
If in a git repo, also appends the first 7 characters of the latest commit.
}
\details{
Useful for watermarking figures, tables, etc.
}
