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
#' @param x A [`data.frame`] with numerical columns. Imputation
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
#'
#' @return An updated version of `x` where missing values have been imputed
#' for each column separately.
#'
#' @examples
#' n <- 30
#' ids_missing <- c(1:2, 10:12, 20:22, (n-1):n)
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
#'   res[[it]] <- impute_df(x, imputation_type = it)
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
  cyclic = FALSE
) {

  imputation_type <- match.arg(imputation_type)
  if (imputation_type == "mean") {
    imputation_span <- round(imputation_span)
  }

  if (imputation_type == "none") {
    return(x)
  }

  cycle <- nrow(x)
  irows <- seq_len(cycle)

  #--- imputations
  icols_withNAs <- which(apply(x, 2, anyNA))

  for (k1 in icols_withNAs) {

    if (imputation_type == "interp") {
      #--- imputation by linear interpolation ------
      rowsets_withNA <- calc_runs(is.na(x[, k1]))
      irows_withNA <- unlist(rowsets_withNA)
      irows_has <- setdiff(irows, irows_withNA)

      for (k2 in seq_along(rowsets_withNA)) {
        ids <- rowsets_withNA[[k2]]
        irows_prev <- irows_has[irows_has < ids[[1L]]]
        irows_next <- irows_has[irows_has > ids[[length(ids)]]]
        nprev <- length(irows_prev)
        nnext <- length(irows_next)

        if (nprev == 0L && nnext == 0L) next

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
            1L + round(circ_minus(xtmpr, ids[[1L]], int = cycle))
          } else {
            xtmpr
          }
          # update next (to avoid that second point grabs the same)
          irows_next <- irows_next[-tmp]
          nnext <- length(irows_next)
        }

        xy0 <- c(x = xtmpx, y = x[xtmpr, k1])

        # Identify second point
        if (nnext > 0L) {
          xtmpx <- xtmpr <- irows_next[[1L]]

        } else if (nprev > 0L) {
          tmp <- if (cyclic) 1L else length(irows_prev)
          xtmpr <- irows_prev[[tmp]]
          xtmpx <- if (cyclic) {
            round(circ_add(ids[[length(ids)]], xtmpr, int = cycle))
          } else {
            xtmpr
          }
        }

        xy1 <- c(x = xtmpx, y = x[xtmpr, k1])

        finterp <- function(x, p0 = xy0, p1 = xy1) {
          a <- p0[["y"]] * (p1[["x"]] - x) + p1[["y"]] * (x - p0[["x"]])
          b <- p1[["x"]] - p0[["x"]]
          a / b
        }

        x[ids, k1] <- finterp(ids)
      }

    } else {
      # method is "mean" or "locf"
      irows_withNA <- which(is.na(x[, k1]))

      for (k2 in irows_withNA) {
        if (imputation_type == "mean" && imputation_span > 0) {
          #--- imputation by mean of neighbor values ------
          spank <- imputation_span

          # locate a sufficient number of non-missing neighbors
          repeat {
            tmp <- seq(k2 - spank, k2 + spank)
            if (cyclic) {
              tmp <- 1 + (tmp - 1) %% cycle
            } else {
              tmp <- tmp[tmp %in% irows]
            }
            # Exclude values that were imputed in a previous step
            ids_source <- tmp[!(tmp %in% irows_withNA)]

            if (length(ids_source) < 2 * imputation_span && spank < cycle) {
              spank <- spank + 1
            } else {
              break
            }
          }

          # impute mean of neighbors
          if (length(ids_source) > 0 && all(is.finite(ids_source))) {
            x[k2, k1] <- mean(x[ids_source, k1])
          }

        } else if (imputation_type == "locf") {
          #--- imputation by last-observation carried forward ------
          dlast <- 1

          # locate last non-missing value
          repeat {
            tmp <- k2 - dlast
            if (cyclic) {
              tmp <- 1 + (tmp - 1) %% cycle
            } else {
              tmp <- tmp[tmp %in% irows]
            }
            ids_source <- tmp[!(tmp %in% irows_withNA)]

            if (length(ids_source) == 1 || dlast >= cycle) {
              break
            } else {
              dlast <- dlast + 1
            }
          }

          # impute locf
          if (length(ids_source) > 0 && all(is.finite(ids_source))) {
            x[k2, k1] <- x[ids_source, k1]
          }
        }
      }
    }
  }

  if (anyNA(x)) {
    warning("It was not possible to impute all missing values.")
  }

  x
}
