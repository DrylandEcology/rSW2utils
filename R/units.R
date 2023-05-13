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


#' Converts units of precipitation data
#'
#' @param x A numeric object. Precipitation data in units of \code{unit_from}.
#' @param unit_from A character string. Units of data in \code{x}. Currently,
#'   supported units include "mm/month", "mm month-1", "mm/d", "mm d-1",
#'   "kg/m2/s", "kg m-2 s-1", "mm/s", "mm s-1", "cm/month", "cm month-1".
#' @param unit_to A character string. Units to which data are converted.
#'   Currently, supported unit are "cm day-1", "cm month-1" and
#'   respectively "cm/day", "cm/month".
#' @param dpm A numeric object. Number of days per month in the time series
#'   \code{x}. Only used if one of the units is on a monthly time scale.
#'
#' @return A numeric object of the same size as \code{x} in units of
#'   \code{unit_to}.
#'
#' @export
# nolint start: nonportable_path_linter.
convert_precipitation <- function(
  x,
  unit_from,
  unit_to = c("cm day-1", "cm month-1", "cm/day", "cm/month"),
  dpm = NA
) {

  unit_to <- match.arg(unit_to)

  if (unit_to %in% c("cm month-1", "cm/month")) {
    if (unit_from %in% c("mm/month", "mm month-1")) {
      x / 10
    } else if (unit_from %in% c("mm/d", "mm d-1", "mm/day", "mm day-1")) {
      x * dpm / 10
    } else if (unit_from %in% c("cm/d", "cm d-1", "cm/day", "cm day-1")) {
      x * dpm
    } else if (unit_from %in% c("kg/m2/s", "kg m-2 s-1", "mm/s", "mm s-1")) {
      x * dpm * 8640
    } else if (unit_from %in% c("cm/month", "cm month-1")) {
      x
    } else {
      stop("Unknown precipitation unit: ", unit_from)
    }

  } else if (unit_to %in% c("cm day-1", "cm/day")) {
    if (unit_from %in% c("mm/month", "mm month-1")) {
      x / (dpm * 10)
    } else if (unit_from %in% c("mm/d", "mm d-1", "mm/day", "mm day-1")) {
      x / 10
    } else if (unit_from %in% c("cm/d", "cm d-1", "cm/day", "cm day-1")) {
      x
    } else if (unit_from %in% c("kg/m2/s", "kg m-2 s-1", "mm/s", "mm s-1")) {
      x * 8640
    } else if (unit_from %in% c("cm/month", "cm month-1")) {
      x / dpm
    } else {
      stop("Unknown precipitation unit: ", unit_from)
    }
  }
}
# nolint end: nonportable_path_linter.


#' Converts units of temperature data
#'
#' @param x A numeric object. Temperature data in units of \code{unit_from}.
#' @param unit_from A character string. Units of data in \code{x}. Currently,
#'   supported units include "K", "F", and "C".
#' @param unit_to A character string. Units to which data are converted.
#'   Currently, supported unit is "C".
#'
#' @return A numeric object of the same size as \code{x} in units of
#'   \code{unit_to}.
#'
#' @export
convert_temperature <- function(x, unit_from, unit_to = "C") {
  if (!identical(unit_to, "C")) {
    stop("'convert_temperature': only converts to units of degree Celsius")
  }

  if (identical(unit_from, "K")) {
    x - 273.15
  } else if (identical(unit_from, "F")) {
    (x - 32) * 0.5555556
  } else if (identical(unit_from, "C")) {
    x
  } else {
    stop("Unknown temperature unit: ", unit_from)
  }
}
