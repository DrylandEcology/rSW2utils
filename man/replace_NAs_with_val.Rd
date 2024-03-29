% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bounded_values.R
\name{replace_NAs_with_val}
\alias{replace_NAs_with_val}
\title{Replace \code{NA} with a value}
\usage{
replace_NAs_with_val(x, val_replace)
}
\arguments{
\item{x}{An R object.}

\item{val_replace}{A value that replaces \code{NA} in \code{x}}
}
\description{
Replace \code{NA} with a value
}
\examples{
replace_NAs_with_val(c(1, NA, 0), -9999)

}
