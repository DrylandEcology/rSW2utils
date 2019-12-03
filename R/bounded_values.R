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


#' @export
replace_NAs_with_val <- function(x, val_replace) {
  x[is.na(x)] <- val_replace
  x
}

#' @export
squash_into_low_high <- function(x, val_low = 0, val_low_replace = val_low,
  val_high = 1, val_high_replace = val_high) {
  if (!is.null(val_low)) x[x < val_low] <- val_low_replace
  if (!is.null(val_high)) x[x > val_high] <- val_high_replace
  x
}

#' @export
cut0Inf <- function(x, val = NA) {
  squash_into_low_high(x, val_low = 0, val_low_replace = val,
    val_high = NULL)
}

#' @export
finite01 <- function(x, val_low_replace = 0, val_high_replace = 1) {
  x <- replace_NAs_with_val(x, val_replace = val_low_replace)
  squash_into_low_high(x, val_low = 0, val_low_replace = val_low_replace,
    val_high = 1, val_high_replace = val_high_replace)
}
