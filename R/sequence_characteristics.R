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
calc_starts <- function(x) {
  temp1 <- rle(as.logical(x))
  temp2 <- cumsum(c(0, temp1[["lengths"]])) + 1
  temp2[-length(temp2)][temp1[["values"]]]
}



#' Apply function to (centered) moving window
#'
#' @param x A numeric vector.
#' @param k An integer value. The width of the moving window.
#' @param win_fun A function. The function must return an object of length one
#'   and accept \code{na.rm} as argument.
#' @param na.rm A logical value. Used as argument to \code{win_fun}.
#' @param circular A logical value. If \code{TRUE}, then window wraps around
#'   both ends of \code{x},
#'   e.g., \code{x} represents mean values for day of year.
#'
#' @return A numeric vector of the length of \code{x}.
#'
#' @references  \code{\link[raster]{movingFun}}
#'
#' @seealso The package \code{caTools} provides fast versions.
#'
#' @export
moving_function <- function(x, k = 3, win_fun = sum, na.rm = FALSE,
  circular = FALSE) {

  k <- round(abs(k))
  stopifnot(
    length(
      win_fun(x[seq_len(min(k, length(x)))], na.rm = na.rm)
    ) == 1
  )

  n <- length(x)
  k2 <- floor(k / 2)

  if (k2 > 0) {
    x <- if (circular) {
      c(x[(n - k2 + 1):n], x, x[seq_len(k2)])
    } else {
      tmp <- rep(NA, k2)
      c(tmp, x, tmp)
    }
  }

  tmp <- matrix(NA, ncol = k, nrow = n)
  for (i in seq_len(k)) {
    tmp[, i] <- x[i:(n + i - 1)]
  }


  apply(tmp, MARGIN = 1, FUN = win_fun, na.rm = na.rm)
}


#' @export
max_duration <- function(x, target_val = 1L, return_doys = FALSE) {
  r <- rle(x)
  rgood <- r[["values"]] == target_val
  igood <- which(rgood)

  if (length(igood) > 0) {
    len <- max(r[["lengths"]][igood])

    if (return_doys) {
      imax <- which(rgood & r[["lengths"]] == len)[1]

      rdoys <- cumsum(r[["lengths"]])
      doys <- if (imax == 1L) {
        c(start = 1L, end = rdoys[1])
      } else {
        c(start = rdoys[imax - 1] + 1,
          end = rdoys[imax])
      }
    }

  } else {
    len <- 0L
    doys <- c(start = NA, end = NA)
  }

  if (return_doys)
    return(c(len, doys))

  len
}
