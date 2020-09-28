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



#' Imputes missing values in a \code{data.frame}
#'
#' @param x A \code{\link{data.frame}} with numerical columns. Imputation
#'   works on each column separately.
#' @param imputation_type A character string describing the imputation method;
#'   currently, one of three values: \describe{
#'     \item{\var{"none"}}{no imputation is carried out;}
#'     \item{\var{"mean"}}{missing values will be replaced by the average
#'       of \code{imputation_span} non-missing values before and
#'       \code{imputation_span} non-missing values after; note:
#'       this may fail if there are less than \code{2 * imputation_span}
#'       non-missing values;}
#'     \item{\var{"locf"}}{missing values will be replaced by the
#'       \var{"last-observation-carried-forward"} imputation method.}
#'  }
#' @param imputation_span An integer value. The number of non-missing values
#'   considered if \code{imputation_type = "mean"}.
#' @param cyclic A logical value. If \code{TRUE}, then the last row of \code{x}
#'   is considered to be a direct neighbor of the first row, e.g., rows of
#'   \code{x} represent day of year for an average year.
#'
#' @return An updated version of \code{x}.
#'
#' @export
impute_df <- function(x, imputation_type = c("none", "mean", "locf"),
  imputation_span = 5L, cyclic = FALSE
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
    irows_withNA <- which(is.na(x[, k1]))

    for (k2 in irows_withNA) {
      if (imputation_type == "mean" && imputation_span > 0) {
        #--- imputation by mean of neighbor values
        spank <- imputation_span

        # locate a sufficient number of non-missing neighbors
        repeat {
          temp <- seq(k2 - spank, k2 + spank)
          if (cyclic) {
            temp <- 1 + (temp - 1) %% cycle
          } else {
            temp <- temp[temp %in% irows]
          }
          ids_source <- temp[!(temp %in% irows_withNA)]

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
        #--- imputation by last-observation carried forward
        dlast <- 1

        # locate last non-missing value
        repeat {
          temp <- k2 - dlast
          if (cyclic) {
            temp <- 1 + (temp - 1) %% cycle
          } else {
            temp <- temp[temp %in% irows]
          }
          ids_source <- temp[!(temp %in% irows_withNA)]

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

  if (anyNA(x)) {
    warning("It was not possible to impute all missing values.")
  }

  x
}
