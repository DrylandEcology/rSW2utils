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


#' Replace \code{NA} with a value
#'
#' @param x An R object.
#' @param val_replace A value that replaces \code{NA} in \code{x}
#'
#' @examples
#' replace_NAs_with_val(c(1, NA, 0), -9999)
#'
#' @export
replace_NAs_with_val <- function(x, val_replace) {
  x[is.na(x)] <- val_replace
  x
}


#' Apply \code{napredict} carefully
#'
#' @inheritParams stats::napredict
#' @param na.index An object that has a length greater than 0 if \code{NA}s
#'   are present in \code{x}.
#' @param na.act An object produced by a
#'   \code{\link[stats]{na.action}} function.
#'
#' @seealso \code{\link[stats]{napredict}}
#'
#' @examples
#' x <- c(3, NA, 2, NA, 4)
#' # Compare
#' cumsum(x)
#'
#' x2 <- stats::na.exclude(x)
#' handle_NAs(cumsum(x2), anyNA(x), attr(x2, "na.action"))
#'
#' @export
handle_NAs <- function(x, na.index, na.act) {
  if (length(na.index) > 0) {
    stats::napredict(na.act, x)
  } else {
    x
  }
}


#' Replace values below or above a threshold (or bound) with new values
#'
#' @param x A vector.
#' @param val_low A value. The lower threshold (or bound).
#' @param val_low_replace A value. The replacement for
#'   values in \code{x} below \code{val_low}.
#' @param val_high A value. The upper threshold (or bound).
#' @param val_high_replace A value. The replacement for
#'   values in \code{x} below \code{val_low}.
#'
#' @examples
#' x <- seq(-5, 5)
#' squash_into_low_high(x)
#' squash_into_low_high(x, val_low = 0, val_high = 3)
#' squash_into_low_high(
#'   x,
#'   val_low = 0, val_low_replace = -999,
#'   val_high = 3, val_high_replace = 999
#' )
#'
#' @name oob
#' @export
squash_into_low_high <- function(
  x,
  val_low = 0,
  val_low_replace = val_low,
  val_high = 1,
  val_high_replace = val_high
) {
  if (!is.null(val_low)) x[x < val_low] <- val_low_replace
  if (!is.null(val_high)) x[x > val_high] <- val_high_replace
  x
}

#' @rdname oob
#' @param val A value. The replacement value.
#'
#' @examples
#' cut0Inf(c(-5, 0, NA, 0.5, 1, 2, Inf))
#'
#' @export
cut0Inf <- function(x, val = NA) {
  squash_into_low_high(
    x,
    val_low = 0,
    val_low_replace = val,
    val_high = NULL
  )
}

#' @rdname oob
#'
#' @examples
#' finite01(c(-5, 0, NA, 0.5, 1, 2, Inf))
#'
#' @export
finite01 <- function(x, val_low_replace = 0, val_high_replace = 1) {
  x <- replace_NAs_with_val(x, val_replace = val_low_replace)
  squash_into_low_high(
    x,
    val_low = 0, val_low_replace = val_low_replace,
    val_high = 1, val_high_replace = val_high_replace
  )
}
