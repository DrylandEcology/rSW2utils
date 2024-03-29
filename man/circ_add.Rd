% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circular_functions.R
\name{circ_add}
\alias{circ_add}
\title{Calculate the circular addition \var{x + y}}
\usage{
circ_add(x, y, int, type = c("minusPiPlusPi", "ZeroPlus2Pi"))
}
\arguments{
\item{x}{A numeric vector or array.}

\item{y}{A numeric vector or array.}

\item{int}{A numeric value. The number of units of \code{x} in a full circle,
e.g., for unit days: \code{int = 365}; for unit months: \code{int = 12}.}

\item{type}{A character string. If \code{type == "minusPiPlusPi"},
then the resulting value lies between \code{-int / 2} and \code{int / 2}.
If \code{type == "ZeroPlus2Pi"},
then the resulting value lies between \code{0} and \code{int}.}
}
\description{
Calculate the circular addition \var{x + y}
}
\examples{
# Matrix examples: day of year
x <- matrix(c(260, 240, 10, 360, 0, 360), nrow = 3, ncol = 2)
y <- matrix(c(240, 260, 360, 10, 360, 0), nrow = 3, ncol = 2)
circ_add(x, y, int = 365)
circ_add(x, y, int = 365, type = "ZeroPlus2Pi")

# Circular addition and subtraction
r1 <- circ_add(circ_minus(x, y, int = 365), y, int = 365)
r2 <- circ_minus(circ_add(x, y, int = 365), y, int = 365)
all.equal(r1, r2)
}
