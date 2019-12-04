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



#' @seealso \code{\link[raster]{movingFun}}
#' @export
window <- function(x, n = 3, win_fun = sum) {
  ids <- seq_len(n)
  lng <- length(x)
  x <- c(x, x[ids])

  m <- matrix(ncol = 3, nrow = lng)
  for (i in ids) {
    m[, i] <- x[i:(lng + i - 1)]
  }

  apply(m, MARGIN = 1, FUN = win_fun)
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
