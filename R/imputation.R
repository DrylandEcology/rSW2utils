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



#' Impute missing values in a data frame by columns
#'
#' @param x A [`data.frame`] or [`matrix`] with numerical columns. Imputation
#' works on each column separately.
#' @param imputation_type A character string describing the imputation method;
#' currently, one of three values:
#' * `"none"`: no imputation is carried out
#' * `"mean"`: missing values will be replaced by the average
#'   of `imputation_span` non-missing values before and
#'   `imputation_span` non-missing values after
#'   note: this may fail if there are less than `2 * imputation_span`
#'   non-missing values
#' * `"locf"`: missing values will be replaced with the
#'   "last-observation-carried-forward"` approach
#' * `"interp"`: missing values will be replaced by linear interpolation
#'   (or extrapolation if at the start or end of a sequence)
#'   using the two closest neighbors
#'   assuming that rows represent equidistant steps
#'   (for each run of missing values separately)
#' @param imputation_span An integer value. The number of non-missing values
#' considered if `imputation_type = "mean"`.
#' @param cyclic A logical value. If `TRUE`, then the last row of `x`
#' is considered to be a direct neighbor of the first row, e.g., rows of
#' `x` represent day of year for an average year.
#' @param nmax_run An integer value. Runs (sets of consecutive missing values)
#' that are equal or shorter to `nmax_run` are imputed;
#' longer runs remain unchanged. Any non-finite value is treated as infinity.
#'
#' @return An updated version of `x` where missing values have been imputed
#' for each column separately.
#'
#' @examples
#' n <- 30
#' ids_missing <- c(1:2, 10:13, 20:22, (n-1):n)
#' x0 <- x <- data.frame(
#'   linear = seq_len(n),
#'   all_missing = NA,
#'   all_same = 1,
#'   cyclic = cos(2 * pi * seq_len(n) / n)
#' )
#' x[ids_missing, ] <- NA
#'
#' res <- list()
#' for (it in c("mean", "locf", "interp")) {
#'   res[[it]] <- impute_df(x, imputation_type = it, nmax_run = 3L)
#'   print(cbind(orig = x0[ids_missing, ], res[[it]][ids_missing, ]))
#' }
#'
#' if (requireNamespace("graphics")) {
#'   par_prev <- graphics::par(mfrow = c(ncol(x) - 1L, 1L))
#'   for (k in seq_len(ncol(x))[-2L]) {
#'     graphics::plot(
#'       x[[k]],
#'       ylim = range(x0[[k]]),
#'       ylab = colnames(x)[[k]],
#'       type = "l"
#'     )
#'     graphics::points(
#'       ids_missing,
#'       x0[ids_missing, k],
#'       pch = 1L,
#'       col = 1L
#'     )
#'     for (it in seq_along(res)) {
#'       graphics::points(
#'         ids_missing,
#'         res[[it]][ids_missing, k],
#'         pch = 1L + it,
#'         col = 1L + it
#'       )
#'     }
#'   }
#'   graphics::par(par_prev)
#' }
#'
#' @export
impute_df <- function(
  x,
  imputation_type = c("none", "mean", "locf", "interp"),
  imputation_span = 5L,
  cyclic = FALSE,
  nmax_run = Inf
) {

  imputation_type <- match.arg(imputation_type)
  if (imputation_type == "mean") {
    imputation_span <- as.integer(round(imputation_span))
  }

  if (isTRUE(!is.finite(nmax_run))) {
    nmax_run <- Inf
  }

  if (
    imputation_type == "none" ||
      (imputation_type == "mean" && imputation_span <= 0L) ||
      nmax_run <= 0L
  ) {
    return(x)
  }

  cycle <- nrow(x)
  irows <- seq_len(cycle)

  #--- imputations
  icols_withNAs <- which(apply(x, 2, anyNA))

  #--- Loop over columns / variables
  for (k1 in icols_withNAs) {

    rowsets_withNA <- calc_runs(is.na(x[, k1, drop = TRUE]))
    irows_withNA <- unlist(rowsets_withNA)
    irows_has0 <- setdiff(irows, irows_withNA)

    #--- Loop over sets of missing
    for (k2 in seq_along(rowsets_withNA)) {

      if (length(rowsets_withNA[[k2]]) > nmax_run) next

      res <- do.call(
        what = switch(
          EXPR = imputation_type,
          interp = impute_run_interp,
          mean = impute_run_mean,
          locf = impute_run_locf
        ),
        args = list(
          v = x[, k1, drop = TRUE],
          rna = rowsets_withNA[[k2]],
          iv = irows_has0,
          cyclic = cyclic,
          span = imputation_span
        )
      )

      if (is.null(res)) next

      x[rowsets_withNA[[k2]], k1] <- res
    }
  }

  if (anyNA(x)) {
    warning("It was not possible to impute all missing values.")
  }

  x
}

#' @param v A numeric vector of all values including missing.
#' @param rna An integer vector of positions in `v` for
#' one sequence of missing values that is to be imputed by interpolation.
#' @param iv An integer vector of positions in `v` with non-missing values.
#' @param ... Additional arguments are silently ignores.
#' @inheritParams impute_df
#'
#' @return Imputed values at positions `rna` or `NULL` if `v` does not carry
#' sufficient information for imputation.
#'
#' @name impute_runs
#' @noRd
NULL

#' Impute a run of missing values with linear interpolation
#' @rdname impute_runs
#' @noRd
impute_run_interp <- function(
  v,
  rna,
  iv,
  cyclic,
  ...
) {

  irows_prev <- iv[iv < rna[[1L]]]
  irows_next <- iv[iv > rna[[length(rna)]]]
  nprev <- length(irows_prev)
  nnext <- length(irows_next)

  if (nprev == 0L && nnext == 0L) return(NULL)

  cycle <- if (cyclic) length(v)

  # Identify first point
  #   - last previous point (if cyclic grab last "next" point);
  #   - otherwise, first next point (if not cyclic, i.e., extrapolation)
  if (nprev > 0L) {
    xtmpx <- xtmpr <- irows_prev[[nprev]]
    # update previous (to avoid that second point grabs the same)
    irows_prev <- irows_prev[-nprev]
    nprev <- length(irows_prev)

  } else if (nnext > 0L) {
    tmp <- if (cyclic) length(irows_next) else 1L
    xtmpr <- irows_next[[tmp]]
    xtmpx <- if (cyclic) {
      1L + round(circ_minus(xtmpr, rna[[1L]], int = cycle))
    } else {
      xtmpr
    }
    # update next (to avoid that second point grabs the same)
    irows_next <- irows_next[-tmp]
    nnext <- length(irows_next)
  }

  p0 <- c(x = xtmpx, y = unname(v[[xtmpr]]))

  # Identify second point
  if (nnext > 0L) {
    xtmpx <- xtmpr <- irows_next[[1L]]

  } else if (nprev > 0L) {
    tmp <- if (cyclic) 1L else length(irows_prev)
    xtmpr <- irows_prev[[tmp]]
    xtmpx <- if (cyclic) {
      round(circ_add(rna[[length(rna)]], xtmpr, int = cycle))
    } else {
      xtmpr
    }
  }

  p1 <- c(x = xtmpx, y = unname(v[[xtmpr]]))

  # Linear interpolation
  a <- p0[["y"]] * (p1[["x"]] - rna) + p1[["y"]] * (rna - p0[["x"]])
  b <- p1[["x"]] - p0[["x"]]
  a / b
}


#' Impute a run of missing values by the mean across adjacent values
#' @rdname impute_runs
#' @noRd
impute_run_mean <- function(
  v,
  rna,
  iv,
  cyclic,
  span,
  ...
) {
  cycle <- length(v)
  ids <- seq_len(cycle)
  res <- rep(NA, length = length(rna))

  for (k2 in seq_along(rna)) {
    # locate a sufficient number of non-missing neighbors
    spank <- span

    repeat {
      tmp <- seq(rna[[k2]] - spank, rna[[k2]] + spank)
      if (cyclic) {
        tmp <- 1L + (tmp - 1L) %% cycle
      } else {
        tmp <- intersect(tmp, ids)
      }
      # Exclude values that were imputed in a previous step
      ids_source <- intersect(tmp, iv)

      if (length(ids_source) < 2L * span && spank < cycle) {
        spank <- spank + 1L
      } else {
        break
      }
    }

    # impute mean of neighbors
    if (length(ids_source) > 0L && all(is.finite(ids_source))) {
      res[[k2]] <- mean(v[ids_source])
    }
  }

  res
}


#' Impute a run of missing values by last observation carried forward
#' @rdname impute_runs
#' @noRd
impute_run_locf <- function(
  v,
  rna,
  iv,
  cyclic,
  ...
) {
  cycle <- length(v)
  ids <- seq_len(cycle)

  # locate last non-missing value
  dlast <- 1L
  repeat {
    tmp <- rna[[1L]] - dlast
    if (cyclic) {
      tmp <- 1L + (tmp - 1L) %% cycle
    } else {
      tmp <- intersect(tmp, ids)
    }
    # Exclude values that were imputed in a previous step
    ids_source <- intersect(tmp, iv)

    if (length(ids_source) == 1L || dlast >= cycle) {
      break
    } else {
      dlast <- dlast + 1L
    }
  }

  # impute locf
  if (
    length(ids_source) == 1L &&
      all(is.finite(ids_source)) &&
      dlast < cycle
  ) {
    rep(v[[ids_source]], length(rna))

  } else {
    res <- vector(mode = typeof(v), length = length(rna))
    res[] <- NA # nolint: extraction_operator_linter.
    res
  }
}
