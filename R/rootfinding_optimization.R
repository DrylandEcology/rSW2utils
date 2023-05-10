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



#' Locate zeros (roots) of a function
#'
#' Find the x-axis values where \code{f(x) = 0}. This can also be used to
#' find the intersections between two functions, i.e., \code{f1(x) - f2(x) = 0},
#' see examples.
#'
#' @param f Function.
#' @param xlim A numeric vector of length 2. The search interval.
#' @param tol A numeric value. The convergence tolerance.
#' @param expected_nroots Maximum number of roots,
#'   i.e., the interval \code{xlim} is split into this number of
#'   contiguous sections and each is checked for the presence of a root
#'
#' @return A list where each element is an object
#'   returned from \code{\link[stats]{uniroot}} or of
#'   class \code{"try-error"} (see \code{\link[base]{try}}), or \code{NULL}.
#'
#' @seealso \code{\link[stats]{uniroot}} and
#'   \code{\link[rootSolve]{uniroot.all}}.
#'
#' @examples
#' # Locate roots of a function
#' f <- function(x) -1 + x^2
#' xt <- seq(-1.5, 1.5, by = 0.1)
#' plot(xt, f(xt), type = "l")
#' abline(h = 0, col = "gray")
#'
#' r <- uniroots(
#'   f = f,
#'   xlim = c(-10, 10),
#'   expected_nroots = 2
#' )
#'
#' for (i in seq_along(r)) {
#'   abline(v = r[[i]][["root"]], col = "red")
#' }
#'
#' # Locate intersections between two functions
#' a2 <- 4
#' f1 <- function(x) sin(2 * x)
#' f2 <- function(x, a) cos(1 - a * x)
#'
#' xt <- seq(-1, 1, by = 0.1)
#' plot(xt, f1(xt), type = "l")
#' lines(xt, f2(xt, a2), col = "gray")
#'
#' r <- uniroots(
#'   f = function(x, a = a2) f1(x) - f2(x, a = a),
#'   xlim = c(-1, 1),
#'   expected_nroots = 3
#' )
#'
#' for (i in seq_along(r)) {
#'   abline(v = r[[i]][["root"]], col = "red")
#' }
#'
#' @export
uniroots <- function(
  f,
  xlim,
  tol = .Machine[["double.eps"]]^0.25,
  expected_nroots = 1
) {

  if (expected_nroots <= 1) {
    list(
      try(
        stats::uniroot(
          f = f,
          interval = xlim,
          tol = tol,
          check.conv = TRUE
        ),
        silent = TRUE
      )
    )
  } else {
    # based on `rootSolve::uniroot.all` but with convergence checks and
    # identical output structure as `stats::uniroot`
    xseq <- seq(min(xlim), max(xlim), len = expected_nroots + 1)
    mod <- f(xseq)
    iz <- which(mod == 0)
    if (length(iz) > 0) {
      Equi <- lapply(
        iz,
        function(i) list(root = xseq[i], f.root = 0, iter = 0)
      )
    } else {
      Equi <- NULL
    }
    ids <- seq_len(expected_nroots)
    ss <- mod[ids] * mod[1 + ids]
    ii <- which(ss < 0)
    for (i in ii) {
      tmp <- list(
        try(
          stats::uniroot(
            f = f,
            lower = xseq[i],
            upper = xseq[i + 1],
            tol = tol,
            check.conv = TRUE
          ),
          silent = TRUE
        )
      )

      if (has_uniroots(tmp, tol)) {
        Equi <- c(Equi, tmp)
      }
    }

    if (is.null(Equi)) list(NULL) else Equi
  }
}

#' Determine whether result(s) of \code{uniroots} was successful
#'
#' @param x An object returned from \code{uniroots}
#' @param tol A numeric value. The convergence tolerance.
#'
#' @return A logical value or a vector of logical values.
#'
#' @examples
#' r1 <- uniroots(
#'   f = function(x) -1 + x^2,
#'   xlim = c(-10, 10),
#'   expected_nroots = 2
#' )
#'
#' has_uniroots(r1)
#'
#' r2 <- uniroots(f = function(x) -1 + x^2, xlim = c(-10, -5))
#' has_uniroots(r2)
#'
#' r3 <- uniroots(f = function(x) Inf, xlim = c(-10, -5))
#' has_uniroots(r3)
#' @export
has_uniroots <- function(x, tol = .Machine[["double.eps"]]^0.25) {
  res <- !is.null(x) && !inherits(x, "try-error")

  if (res) {
    res <- sapply(x, function(x) !is.null(x) && !inherits(x, "try-error"))
    inoerror <- res

    if (any(inoerror)) {
      res[inoerror] <- sapply(
        x[inoerror],
        function(xi) xi[["iter"]] < 1000 && abs(xi[["f.root"]]) < tol
      )
    }
  }

  res
}
