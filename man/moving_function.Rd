% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequence_characteristics.R
\name{moving_function}
\alias{moving_function}
\title{Apply function to (centered) moving window}
\usage{
moving_function(x, k = 3, win_fun = sum, na.rm = FALSE, circular = FALSE)
}
\arguments{
\item{x}{A numeric vector.}

\item{k}{An integer value. The width of the moving window.}

\item{win_fun}{A function. The function must return an object of length one
and accept \code{na.rm} as argument.}

\item{na.rm}{A logical value. Used as argument to \code{win_fun}.}

\item{circular}{A logical value. If \code{TRUE}, then window wraps around
both ends of \code{x},
e.g., \code{x} represents mean values for day of year.}
}
\value{
A numeric vector of the length of \code{x}.
}
\description{
Apply function to (centered) moving window
}
\references{
\code{\link[raster]{movingFun}}
}
\seealso{
The package \code{caTools} provides fast versions.
}
