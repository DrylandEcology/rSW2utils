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

#' 2-dimensional line function
#'
#' The line connects two points \code{c(x0, y0)} and \code{c(x1, y1)}.
#'
#' @param x0 A numeric value. The x-axis value of the first point.
#' @param y0 A numeric value. The y-axis value of the first point.
#' @param x1 A numeric value. The x-axis value of the second point.
#' @param y1 A numeric value. The y-axis value of the second point.
#'
#' @examples
#' xs <- seq(-2, 2, length = 100)
#' lfun <- f_2pline(1, 1, -1, 5)
#' plot(lfun(xs), type = "l")
#' points(1, 1)
#' points(-1, 5)
#'
#' lines(f_2pline(1, 1, 2, 1)(xs)) # horizonatal line
#' lines(f_2pline(1, 1, 1, 2)(xs)) # vertical line doesn't work
#'
#' @export
f_2pline <- function(x0, y0, x1, y1) {
  function(x) {
    N <- length(x)

    tmp <- if (abs(x1 - x0) < rSW2_glovars[["tol"]]) {
      # vertical line
      rep(NA, N)

    } else if (abs(y1 - y0) < rSW2_glovars[["tol"]]) {
      # horizontal line
      rep(y0, N)

    } else {
      y0 + (y1 - y0) / (x1 - x0) * (x - x0)
    }

    matrix(
      c(x = x, y = tmp),
      nrow = N,
      ncol = 2
    )
  }
}

#' Circle function with angle argument \code{theta}
#'
#' Circle function with angle argument \code{theta} such that
#' \code{sign(dir) * theta + theta0 = 0} corresponds to \code{c(x0 + r, y0)}
#'
#' @param x0 A numeric value. The x-axis value of the circle center.
#' @param y0 A numeric value. The y-axis value of the circle center.
#' @param r A numeric value. The circle radius.
#' @param theta0 A numeric value. The (anti-clockwise) angle/rotation of the
#'   starting point of the circle.
#' @param dir A numeric value, interpreted as either
#'  \code{-1}, i.e., clockwise, or as \code{1}, i.e., anti-clockwise
#'  direction.
#'
#' @examples
#' thetas <- seq(0, 2 * pi, length = 200)
#' # circle with radius 1 and center at 0/0
#' circle1 <- f_circle()
#' plot(circle1(thetas), asp = 1, type = "l")
#' points(0, 0, pch = 4)
#' # circle with radius 0.1 and center at 0.3/0.3
#' lines(f_circle(0.3, 0.3, 0.1)(thetas))
#' # circle with radius 0.1 and center at -0.3/0.3
#' lines(f_circle(-0.3, 0.3, 0.1)(thetas))
#' # semi-circle with radius 0.5 and center at 0/0
#' lines(f_circle(0, 0, 0.5, dir = -1)(seq(0, pi, length = 200)))
#'
#' @export
f_circle <- function(x0 = 0, y0 = 0, r = 1, theta0 = 0, dir = 1) {
  dir <- as.numeric(dir)
  dir <- if (isTRUE(dir > 0)) 1 else -1

  function(theta) {
    tmp <- dir * theta + theta0
    matrix(
      c(x = x0 + r * cos(tmp), y = y0 + r * sin(tmp)),
      nrow = length(theta),
      ncol = 2
    )
  }
}
