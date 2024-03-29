% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequence_characteristics.R
\name{runs}
\alias{runs}
\alias{calc_starts}
\alias{calc_runs}
\title{Calculate positional information about \code{TRUE}-runs}
\usage{
calc_starts(x)

calc_runs(x)
}
\arguments{
\item{x}{A vector. Values are coerced to logical values.}
}
\value{
For \code{\link[=calc_starts]{calc_starts()}}:
an integer vector with start position(s) of \code{TRUE}-runs.

For \code{\link[=calc_runs]{calc_runs()}}:
a list with integer sequences of the position(s) of \code{TRUE}-runs.
}
\description{
Calculate positional information about \code{TRUE}-runs
}
\examples{
calc_starts(c(0, 1, 1, 0, 0)) ## expected: 2
calc_runs(c(0, 1, 1, 0, 0)) ## expected: list(2:3)
calc_starts(c(1, 1, 0, 0)) ## expected: 1
calc_runs(c(1, 1, 0, 0)) ## expected: list(1:2)
calc_starts(c(1, 1, 0, 0, 1, 1)) ## expected: 1 5
calc_runs(c(1, 1, 0, 0, 1, 1)) ## expected: list(1:2, 5:6)
calc_starts(c(1.5, 2, 0, 0, 15, 3.5)) ## expected: 1 5
calc_runs(c(1.5, 2, 0, 0, 15, 3.5)) ## expected: list(1:2, 5:6)
calc_starts(c(0, 0)) ## expected: integer(0)
calc_runs(c(0, 0)) ## expected: list()

}
