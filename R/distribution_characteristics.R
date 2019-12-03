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


#' Find the most common element of x
#' @export
majority <- function(x, na.rm = FALSE) {
  ina <- is.na(x)
  if (sum(ina) >= length(x)) {
    res <- NA
  } else {
    useMode <- FALSE
    naVal <- NULL
    if (na.rm) {
      x <- stats::na.exclude(x)
      useMode <- TRUE
    }
    if (!na.rm && sum(ina) > 0 && inherits(x, "numeric")) {
      naVal <- min(x, na.rm = TRUE) - 100
      x[ina] <- naVal
      useMode <- TRUE
    }
    if (useMode && requireNamespace("statip")) {
      res <- statip::mfv(x)
    } else {
      temp <- table(x, useNA = if (na.rm) "no" else "always")
      res <- names(temp)[which(c(temp) == max(c(temp)))]
    }

    if (length(res) > 1) res <- res[sample(x = length(res), size = 1)]
    if (useMode && !is.null(res) && isTRUE(res == naVal)) res <- NA
  }

  if (is.factor(x)) {
    factor(res, levels = levels(x), ordered = is.ordered(x))
  } else {
    as.vector(res, mode = typeof(x))
  }
}
