% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/add_description.R
\name{format_code}
\alias{format_code}
\title{format_code}
\usage{
format_code(indent = 2, arrow = TRUE, recursive = TRUE)
}
\arguments{
\item{indent}{integer; 2 spaces indentation}

\item{arrow}{logical; TRUE default means use arrows for assignments}

\item{recursive}{logical; TRUE default means that anything in R code files will be formatted.}
}
\description{
A convenience wrapper to format R code according to CIP standards for R
(largely based on H. Wickham/GNU C coding) conventions.
}
\author{
Reinhard Simon
}

