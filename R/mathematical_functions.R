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


#' Test whether input represents a natural number
#' @param x An integer, numeric, or complex vector, matrix, or array.
#' @return A logical value.
#' @export
is.natural <- function(x) {
  typeof(x) %in% c("integer", "double", "complex") &&
    !is.null(x) && length(x) > 0 && !anyNA(x) &&
    isTRUE(all.equal(x, round(x))) && all(x > 0)
}


#' Correlation function with safe-guards against errors
#'
#' @inheritParams stats::cor
#'
#' @section Note: If \code{x} is a two-dimensional object and \code{y} is
#'   \code{NULL} (or missing), then the correlation between the first and second
#'   column of \code{x} is calculated -- unlike \code{\link[stats]{cor}} which
#'   calculates all pairwise correlations among all columns of \code{x}.
#'
#' @return The value of \code{\link[stats]{cor}} if successful,
#'   otherwise \code{NA}.
#'
#' @examples
#' x <- sample(10)
#' x <- data.frame(
#'   val1 = x,
#'   val2 = x + stats::rnorm(10, 0, 1)
#' )
#' cor2(x)
#'
#' @export
cor2 <- function(x, y = NULL, use = "everything",
  method = c("pearson", "kendall", "spearman")) {

  if (is.null(y)) {
    y <- x[, 2]
    x <- x[, 1]
  }

  res <- try(
    stats::cor(x = x, y = y, use = use, method = method),
    silent = TRUE
  )

  if (inherits(res, "try-error")) NA else res
}




#' The intersection on any number of vectors
#'
#' @param \dots Any number of vectors or a list of vectors.
#' @return A vector of the same mode as inputs.
#' @seealso \code{\link{intersect}}
#' @export
intersect2 <- function(...) {
  x <- list(...)
  n <- length(x)

  if (is.list(x[[1]]) && n == 1) {
    x <- x[[1]]
    n <- length(x)
  }

  res <- NULL
  if (n > 1) {
    if (all(lengths(x)) > 0) {
      res <- x[[1]]
      for (k in 2:n) {
        res <- intersect(res, x[[k]])
      }
    }

  } else {
    res <- x[[1]]
  }

  res
}


#' Scale values of a vector so that the result sums to 1
#'
#' @param x A numeric vector
#'
#' @export
scale_by_sum <- function(x) {
  temp <- sum(x, na.rm = TRUE)
  if (temp > 0 && is.finite(temp)) {
    x / temp
  } else {
    x
  }
}


#' Check that values in matrix-like object are (strictly) monotonically
#' increasing/decreasing
#'
#' @param x A numeric matrix like object.
#' @param MARGIN An integer value giving the subscripts over which the
#'   monotonicity will be checked; 1 indicates rows, 2 indicates columns.
#' @param increase A logical value. If \code{TRUE}, check monotonic increase; if
#'   \code{FALSE}, check monotonic decrease.
#' @param strictly A logical value. If \code{TRUE}, check for a strict monotonic
#'   pattern.
#' @param fail A logical value. If \code{TRUE}, throw error if monotonic check
#'   fails.
#' @param replacement A value that replaces non-(strictly) monotonically
#'   increasing/decreasing values if \code{fail} is \code{FALSE}.
#' @param na.rm A logical value. If \code{TRUE}, then ignore \code{NA}s; if
#'   \code{FALSE}, then fail if \code{strictly} or replace with
#'   \code{replacement}.
#' @return The updated \code{x}.
#' @export
check_monotonic_increase <- function(x, MARGIN = 1, increase = TRUE,
  strictly = FALSE, fail = FALSE, replacement = NA, na.rm = FALSE) {

  stopifnot(MARGIN %in% c(1, 2), length(dim(x)) == 2)

  x <- as.matrix(x)
  if (MARGIN == 2) {
    x <- t(x)
  }

  mfun <- if (increase) {
    if (strictly) ">" else ">="
  } else {
    if (strictly) "<" else "=<"
  }

  ord <- !match.fun(mfun)(x[, -1, drop = FALSE], x[, -ncol(x), drop = FALSE])

  if ((!na.rm && strictly && anyNA(x)) || any(ord, na.rm = TRUE)) {
    if (fail) {
      stop(paste0("'check_monotonic_increase': data are not ",
        if (strictly) "strictly ", "monotonically ",
        if (increase) "increasing " else "decreasing ",
        if (MARGIN == 1) "in rows." else "in columns."))

    } else {
      x[, -1][is.na(ord) | ord] <- replacement
      x[is.na(x[, 1]), 1] <- replacement
    }
  }

  if (MARGIN == 1) x else t(x)
}
