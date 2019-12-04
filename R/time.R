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

#' Check whether a Gregorian calendar year is a leap year
#'
#' @param y An integer value. A Gregorian calendar year.
#'
#' @examples
#' isFALSE(isLeapYear(1900))
#' isTRUE(isLeapYear(1980))
#' isTRUE(isLeapYear(2000))
#'
#' @export
isLeapYear <- function(y) {
  #from package: tis
  y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
}


#' Create a sequence of each day between and including two calendar years
#'
#' @param start_year An integer value. The first year.
#' @param end_year An integer value. The last year.
#'
#' @examples
#' length(days_in_years(1980, 1980)) == 366
#'
#' @export
days_in_years <- function(start_year, end_year) {
  seq(
    from = ISOdate(start_year, 1, 1, tz = "UTC"),
    to = ISOdate(end_year, 12, 31, tz = "UTC"),
    by = "1 day"
  )
}



#' The sequence of month numbers for each day in the period from - to
#'
#' @param from A list with three elements \code{year}, \code{month},
#'   and \code{day}.
#' @param to A list with three elements \code{year}, \code{month},
#'   and \code{day}.
#' @inheritParams base::as.POSIXlt
#'
#' @seealso \code{\link[base]{as.POSIXlt}}, \code{\link[base]{seq.POSIXt}}
#'
#' @examples
#' \dontrun{
#'  month1 <- function() as.POSIXlt(seq(from = ISOdate(1980, 1, 1, tz = "UTC"),
#'     to = ISOdate(2010, 12, 31, tz = "UTC"), by = "1 day"))$mon + 1
#'  month2 <- function() seq_month_ofeach_day(
#'    from = list(1980, 1, 1),
#'    to = list(2010, 12, 31),
#'    tz = "UTC"
#'  )
#'
#'  if (requireNamespace("microbenchmark", quietly = TRUE))
#'    # barely any difference
#'    microbenchmark::microbenchmark(month1(), month2())
#'  }
#' @export
seq_month_ofeach_day <- function(from = list(year = 1900, month = 1, day = 1),
  to = list(year = 1900, month = 12, day = 31), tz = "UTC") {

  x <- paste(from[[1]], from[[2]], from[[3]], 12, 0, 0, sep = "-")
  from0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS",
    tz = tz)))
  x <- paste(to[[1]], to[[2]], to[[3]], 12, 0, 0, sep = "-")
  to0 <- unclass(as.POSIXct.POSIXlt(strptime(x, "%Y-%m-%d-%H-%M-%OS", tz = tz)))

  res <- seq.int(0, to0 - from0, by = 86400) + from0
  as.POSIXlt.POSIXct(.POSIXct(res, tz = tz))$mon + 1
}
