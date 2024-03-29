% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputation.R
\name{impute_df}
\alias{impute_df}
\title{Impute missing values in a data frame by columns}
\usage{
impute_df(
  x,
  imputation_type = c("none", "mean", "locf", "interp"),
  imputation_span = 5L,
  cyclic = FALSE,
  nmax_run = Inf
)
}
\arguments{
\item{x}{A \code{\link{data.frame}} or \code{\link{matrix}} with numerical columns. Imputation
works on each column separately.}

\item{imputation_type}{A character string describing the imputation method;
currently, one of three values:
\itemize{
\item \code{"none"}: no imputation is carried out
\item \code{"mean"}: missing values will be replaced by the average
of \code{imputation_span} non-missing values before and
\code{imputation_span} non-missing values after
note: this may fail if there are less than \code{2 * imputation_span}
non-missing values
\item \code{"locf"}: missing values will be replaced with the
"last-observation-carried-forward"` approach
\item \code{"interp"}: missing values will be replaced by linear interpolation
(or extrapolation if at the start or end of a sequence)
using the two closest neighbors
assuming that rows represent equidistant steps
(for each run of missing values separately)
}}

\item{imputation_span}{An integer value. The number of non-missing values
considered if \code{imputation_type = "mean"}.}

\item{cyclic}{A logical value. If \code{TRUE}, then the last row of \code{x}
is considered to be a direct neighbor of the first row, e.g., rows of
\code{x} represent day of year for an average year.}

\item{nmax_run}{An integer value. Runs (sets of consecutive missing values)
that are equal or shorter to \code{nmax_run} are imputed;
longer runs remain unchanged. Any non-finite value is treated as infinity.}
}
\value{
An updated version of \code{x} where missing values have been imputed
for each column separately.
}
\description{
Impute missing values in a data frame by columns
}
\examples{
n <- 30
ids_missing <- c(1:2, 10:13, 20:22, (n-1):n)
x0 <- x <- data.frame(
  linear = seq_len(n),
  all_missing = NA,
  all_same = 1,
  cyclic = cos(2 * pi * seq_len(n) / n)
)
x[ids_missing, ] <- NA

res <- list()
for (it in c("mean", "locf", "interp")) {
  res[[it]] <- impute_df(x, imputation_type = it, nmax_run = 3L)
  print(cbind(orig = x0[ids_missing, ], res[[it]][ids_missing, ]))
}

if (requireNamespace("graphics")) {
  par_prev <- graphics::par(mfrow = c(ncol(x) - 1L, 1L))
  for (k in seq_len(ncol(x))[-2L]) {
    graphics::plot(
      x[[k]],
      ylim = range(x0[[k]]),
      ylab = colnames(x)[[k]],
      type = "l"
    )
    graphics::points(
      ids_missing,
      x0[ids_missing, k],
      pch = 1L,
      col = 1L
    )
    for (it in seq_along(res)) {
      graphics::points(
        ids_missing,
        res[[it]][ids_missing, k],
        pch = 1L + it,
        col = 1L + it
      )
    }
  }
  graphics::par(par_prev)
}

}
