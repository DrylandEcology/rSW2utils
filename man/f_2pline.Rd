% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curves.R
\name{f_2pline}
\alias{f_2pline}
\title{2-dimensional line function}
\usage{
f_2pline(x0, y0, x1, y1)
}
\arguments{
\item{x0}{A numeric value. The x-axis value of the first point.}

\item{y0}{A numeric value. The y-axis value of the first point.}

\item{x1}{A numeric value. The x-axis value of the second point.}

\item{y1}{A numeric value. The y-axis value of the second point.}
}
\description{
The line connects two points \code{c(x0, y0)} and \code{c(x1, y1)}.
}
\examples{
xs <- seq(-2, 2, length = 100)
lfun <- f_2pline(1, 1, -1, 5)
plot(lfun(xs), type = "l")
points(1, 1)
points(-1, 5)

lines(f_2pline(1, 1, 2, 1)(xs)) # horizonatal line
lines(f_2pline(1, 1, 1, 2)(xs)) # vertical line doesn't work

}
