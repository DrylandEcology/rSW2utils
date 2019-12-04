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


#' Error function
#' @seealso Code is from examples of \code{\link[stats]{pnorm}}.
#' @param x A numeric vector.
#' @return A numeric vector of the size of \code{x}.
erf <- function(x) 2 * stats::pnorm(x * sqrt(2)) - 1


#' Check that data are within range of normal distribution
#'
#' @param data A numeric vector. Daily values of temperature.
#' @param sigmaN An integer value. A multiplier of \code{stats::sd}.
#' @export
test_sigmaNormal <- function(data, sigmaN = 6) {
  md <- mean(data)
  sdd <- stats::sd(data) * sigmaN
  stopifnot(data < md + sdd, data > md - sdd)
}


#' Check that data are within range of an approximated gamma distribution
#'
#' @section Note: Approximate shape and scale instead of very slow call:
#'   \code{g <- MASS::fitdistr(data, "gamma")}
#' @param data A numeric vector.
#' @param sigmaN An integer value. A multiplier of \code{stats::sd}.
#' @references Choi, S. C., and R. Wette. 1969. Maximum Likelihood Estimation of
#'   the Parameters of the Gamma Distribution and Their Bias. Technometrics
#'   11:683-690.
#' @export
test_sigmaGamma <- function(data, sigmaN = 6) {
  tempD <- data[data > 0]

  if (length(tempD) >= 2 && stats::sd(tempD) > 0) {
    tempM <- mean(tempD)
    temp <- log(tempM) - mean(log(tempD))
    gshape <- (3 - temp + sqrt((temp - 3) ^ 2 + 24 * temp)) / (12 * temp)
    gscale <- tempM / gshape
    stopifnot(data < stats::qgamma(erf(sigmaN / sqrt(2)), shape = gshape,
      scale = gscale))
  }
}



#' Find the most common element of a vector
#'
#' @param x A vector of numeric, logical, or character mode and it may be a
#'   factor as well.
#' @param na.rm A logical value
#'
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
