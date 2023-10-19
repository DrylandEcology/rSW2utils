################################################################################
# rSW2utils: Utility tools for SOILWAT2 and STEPWAT2 simulation experiments.
# Copyright (C) 2019 Daniel Schlaepfer, John Bradford, William Lauenroth,
#   Kyle Palmquist
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################



#' Bag of little bootstraps
#'
#' Bag of little bootstrap as described by Kleiner et al. 2014 implemented
#' with adaptive convergence checking.
#'
#' @param data A two-dimensional numerical data object.
#' @param subset_size_b An integer value. The number of rows for each subset
#'   bootstraps. Kleiner et al. 2014 suggest empirically a value of
#'   \code{nrow(data) ^ lambda} with \code{lambda = 0.7}, but
#'   \code{lambda between 0.1 and 1}.
#' @param n_subsets An integer value. The upper limit of sampled subsets
#'   \var{s}. If \code{NA}, then all subsets are sampled.
#'   If convergence is achieved earlier, then not all \var{s} subsets are
#'   processed.
#' @param n_resamples An integer value. The upper limit of
#'   Monte-Carlo iterations (resamples, \var{r}) carried out on each subset.
#'   Kleiner et al. 2014 found empirically that a value of \var{r = 100}
#'   worked well for confidence intervals. If convergence is achieved earlier,
#'   then not all \var{r} resamples are processed.
#' @param window_subsets An integer value. The window size of the number
#'   of previous subsets to consider for adaptive convergence checking.
#' @param window_resamples An integer value. The window size of the number
#'   of previous resamples to consider for adaptive convergence checking.
#' @param epsilon A positive numerical value. The acceptable relative error
#'   to determine convergence.
#' @param fun_estimator A function with two arguments
#'   \var{x} and \var{weights} where
#'   \var{x} will be \var{j-th} subset of \code{data}
#'   and \var{weights} a vector of counts defining the \var{k-th} resample
#'   of the \var{j-th} subset (see examples). The return value is a named
#'   vector of the estimator(s) of interest.
#' @param fun_metric A function with one argument \var{x} which will be
#'   applied to each element of the results of \code{fun_estimator} across
#'   all resamples of the \var{j-th} subset. The return value is a named
#'   vector of the estimator(s) of quality assessment for the estimator(s) of
#'   interest.
#'
#' @return A two-dimensional object of means across all subsets
#'   where rows represent the estimator(s) of quality assessment,
#'   i.e., the output of \code{fun_metric}, and the columns represent the
#'   the estimator(s) of interest, i.e., the output of \code{fun_estimator}.
#'
#' @seealso Function \code{drBLB} of package
#'   \url{https://github.com/delta-rho/datadr}.
#'
#' @references Kleiner, A., A. Talwalkar, P. Sarkar, and M. I. Jordan. 2014.
#'   A scalable bootstrap for massive data. Journal of the Royal Statistical
#'   Society: Series B (Statistical Methodology) 76:795–816.
#'
#' @examples
#' n <- 10000
#' xt <- seq(0, 10, length.out = n)
#' ex_data <- data.frame(
#'   x1 = sample(xt),
#'   x2 = sample(xt)
#' )
#'
#' # Linear regression with coefficients 1, 2, and 3
#' ex_data[, "y"] <-
#'   1 + rnorm(n, 0, 1) + 2 * ex_data[, "x1"] + 3 * ex_data[, "x2"]
#'
#' # Estimate coefficients with BLB
#' blb(
#'   data = ex_data,
#'   fun_estimator = function(x, weights) {
#'     coef(lm(
#'       y ~ x1 + x2,
#'       data = x,
#'       weights = weights / max(weights)
#'     ))
#'   },
#'   fun_metric = function(x) {
#'     quantile(x, probs = c(0.025, 0.5, 0.975))
#'   }
#' )
#'
#' @export
blb <- function(
  data,
  subset_size_b = nrow(data)^0.7,
  n_subsets = NA,
  n_resamples = 100,
  window_subsets = 3,
  window_resamples = 20,
  epsilon = 0.05,
  fun_estimator = NULL,
  fun_metric = NULL
) {

  stopifnot(
    is.function(fun_estimator),
    is.function(fun_metric)
  )

  # size of data
  n <- nrow(data)

  # disjoint random subsets of the observed data each of
  # approximate size `subset_size_b`
  ids_subsets <- sample(
    x = ceiling(n / subset_size_b),
    size = n,
    replace = TRUE
  )

  # number of sampled subsets
  n_subsets <- if (is.na(n_subsets)) {
    max(ids_subsets)
  } else {
    min(n_subsets, max(ids_subsets))
  }

  # number of resamples per subset
  ids_r <- seq_len(n_resamples)

  # containers for estimators of interests and of quality assessments
  out_quality <- names(fun_metric(stats::runif(10)))
  ntmp <- seq_len(min(10, n))
  out_estimators <- names(suppressWarnings(fun_estimator(data[ntmp, ], ntmp)))

  res_ests <- array(
    NA,
    dim = c(n_subsets, length(out_estimators), n_resamples),
    dimnames = list(NULL, out_estimators, NULL)
  )
  res_qual <- array(
    NA,
    dim = c(n_subsets, length(out_quality), length(out_estimators)),
    dimnames = list(NULL, out_quality, out_estimators)
  )

  # loop over each of the s subsets
  for (j in seq_len(n_subsets)) {
    ids_j <- which(ids_subsets == j)
    b <- length(ids_j)

    # generate the r Monte Carlo resamples
    resample_weights <- stats::rmultinom(
      n = n_resamples,
      size = n,
      prob = rep(1 / b, b)
    )

    # loop over each of the r Monte Carlo resamples/iterations
    for (k in ids_r) {
      # calculate estimator(s) of interest
      res_ests[j, , k] <- suppressWarnings(
        fun_estimator(data[ids_j, ], weights = resample_weights[, k])
      )

      # if estimator(s) of interest have converged then stop resamples
      if (k > window_resamples) {
        z_t <- res_ests[j, , k]
        tmp <- sweep(
          x = res_ests[j, , seq(k - window_resamples, k - 1)],
          MARGIN = 1,
          STATS = z_t,
          FUN = "-"
        )
        tmp <- sweep(abs(tmp), 1, abs(z_t), FUN = "/")
        have_resamples_conv <- all(colMeans(tmp) < epsilon)

        if (have_resamples_conv) break
      }
    }

    if (!have_resamples_conv) {
      warning(
        "Subset ", j, " has not converged: ",
        "number of resamples (r = ", n_resamples, ") may be too small."
      )
    }

    # calculate estimator(s) quality assessment for subset j across resamples
    res_qual[j, , ] <- apply(
      X = res_ests[j, , seq_len(k), drop = FALSE],
      MARGIN = 2,
      FUN = fun_metric
    )

    # if estimator(s) quality assessment have converged then stop subsets
    if (j > window_subsets) {
      z_t <- res_qual[j, , , drop = FALSE]
      tmp <- sweep(
        x = res_qual[seq(j - window_subsets, j - 1), , , drop = FALSE],
        MARGIN = 2:3,
        STATS = z_t,
        FUN = "-"
      )
      tmp <- sweep(abs(tmp), 2:3, abs(z_t), FUN = "/")
      have_subsets_conv <- all(rowMeans(tmp) < epsilon)

      if (have_subsets_conv) break
    }
  }

  if (!have_subsets_conv) {
    warning(
      "BLB has not converged: ",
      "number of subsets (s = ", n_subsets, ") may be too small."
    )
  }

  # Average of quality assessment estimates across the s subsets
  colMeans(res_qual[seq_len(j), , , drop = FALSE])
}
