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



#' Calculate positions of the start of \code{TRUE}-runs
#'
#' @param x A vector. Values are coerced to logical values.
#' @return The start position(s) (base-1) of \code{TRUE}-runs in \code{x}.
#'
#' @examples
#' calc_starts(c(0, 1, 1, 0, 0)) ## expected: 2
#' calc_starts(c(1, 1, 0, 0)) ## expected: 1
#' calc_starts(c(1, 1, 0, 0, 1, 1)) ## expected: 1 5
#' calc_starts(c(1.5, 2, 0, 0, 15, 3.5)) ## expected: 1 5
#' calc_starts(c(0, 0)) ## expected: numeric(0)
#'
#' @export
calc_starts <- function(x) {
  tmp1 <- rle(as.logical(x))
  tmp2 <- cumsum(c(0, tmp1[["lengths"]])) + 1
  tmp2[-length(tmp2)][tmp1[["values"]]]
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
moving_function <- function(
  x,
  k = 3,
  win_fun = sum,
  na.rm = FALSE,
  circular = FALSE
) {

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


#' Determine the length of the (first) longest run
#'
#' @param x A vector.
#' @param target_val A value. Runs of \code{target_val} in \code{x} are used.
#' @param return_doys A logical value.
#'
#' @return A numeric vector with one or three elements including
#'   the length of the longest run of value \code{target_val}
#'   (the first if multiple runs are of equally the longest) and,
#'   if \code{return_doys == TRUE}, then the start and end
#'   position of that longest run.
#'
#' @examples
#' max_duration(0, target_val = 1)
#' x <- c(rep(3, 3), rep(10, 10), 4, rep(10, 20), rep(3, 6))
#' max_duration(x, 10, FALSE)
#' max_duration(x, 10, TRUE)
#'
#' @export
max_duration <- function(x, target_val = 1L, return_doys = FALSE) {
  r <- rle(x)
  rgood <- r[["values"]] == target_val
  igood <- which(rgood)

  if (length(igood) > 0) {
    len <- max(r[["lengths"]][igood])

    if (return_doys) {
      imax <- which(rgood & r[["lengths"]] == len)[[1L]]

      rdoys <- cumsum(r[["lengths"]])
      doys <- if (imax == 1L) {
        c(start = 1L, end = rdoys[[1L]])
      } else {
        c(
          start = rdoys[imax - 1] + 1,
          end = rdoys[imax]
        )
      }
    }

  } else {
    len <- 0L
    doys <- c(start = NA, end = NA)
  }

  if (return_doys) {
    return(c(len, doys))
  }

  len
}


#' Count the number of peaks in a series of values
#'
#' Peaks are identified as patterns of increase, possibly followed by a
#' constant stretch, which ends in a decrease.
#'
#' @param x A numeric vector
#' @param min_change A numeric value. The size of a minimal change between
#'   two consecutive values of \code{x} to count as increase/decrease.
#'
#' @return An integer value representing the number of peaks
#'
#' @examples
#' count_peaks(c(0, 1, 0, 1, 0)) ## expect 2 peaks
#' count_peaks(c(0, 1, 0, 1)) ## expect 1 peak
#' count_peaks(c(1, 0, 1)) ## expect 0 peak
#' count_peaks(c(0, 1, 1, 2.5, 5.1, 4.9)) ## expect 1 peak
#' count_peaks(c(0, 1, 1, 2.5, 5.1, 4.9), min_change = 0.5) ## expect 0 peak
#' count_peaks(c(0, 1, 1, 0.8, 5.1, 4)) ## expect 2 peak
#' count_peaks(c(0, 1, 1, 0.8, 5.1, 4), min_change = 0.1) ## expect 2 peak
#' count_peaks(c(0, 1, 1, 0.8, 5.1, 4), min_change = 0.5) ## expect 1 peak
#'
#' @export
count_peaks <- function(x, min_change = 0) {
  min_change <- abs(min_change)

  dtmp <- diff(x)
  stmp <- sign(dtmp)

  if (min_change > 0) {
    stmp[abs(dtmp) < min_change] <- 0
  }

  rtmp <- rle(stmp)
  tmp <- rtmp[["values"]]
  tmp <- tmp[tmp != 0]
  tmp <- diff(tmp)
  as.integer(sum(tmp == -2))
}



#' Scale values so that they retain the value of a function
#' applied to reference values
#'
#' @param x Numeric vector. Values to scale.
#' @param x_ref Numeric vector.
#'   Values from which the reference value is calculated.
#' @param fun A function or character vector identifying a function.
#' @param na.rm A logical value. Argument is passed to \code{fun}.
#'
#' @seealso \code{\link{scale_to_reference_peak_frequency}}
#' @return A numeric vector of the same length as \code{x}.
#'
#' @examples
#' x <- c(1:6, 8)
#' x0 <- c(10, -1)
#' x0NA <- c(x0, NA)
#'
#' # Scale to retain value of reference maximum
#' scale_to_reference_fun(x, x0, fun = max)
#'
#' # Scale to retain value of reference sum
#' scale_to_reference_fun(x, x0, fun = sum)
#'
#' # Scale to retain value of reference mean
#' scale_to_reference_fun(x, x0, fun = mean)
#'
#' # Scale to retain value of reference length (mostly nonsensical!)
#' scale_to_reference_fun(x, x0, fun = length)
#'
#' # Scale and handle NAs
#' scale_to_reference_fun(x, x0NA, fun = mean, na.rm = FALSE)
#' scale_to_reference_fun(x, x0NA, fun = max, na.rm = TRUE)
#'
#' @export
scale_to_reference_fun <- function(x, x_ref, fun, na.rm = FALSE) {
  fun <- match.fun(fun)

  # Determine whether function accepts a `na.rm` argument
  fun_x <- try(fun(x, na.rm = na.rm), silent = TRUE)

  if (inherits(fun_x, "try-error")) {
    fun_x <- fun(x)
    fun_ref <- fun(x_ref)

  } else {
    fun_ref <- fun(x_ref, na.rm = na.rm)
  }

  # Calculate scaled values
  x * fun_ref / fun_x
}

#' Scale values so that the frequency of values < max(reference values) is the
#' same as the frequency of reference values < max(reference values)
#'
#' @inheritParams scale_to_reference_fun
#' @param cap_at_peak A logical value. If \code{TRUE}, then scaled values
#'   larger than the reference peak value are reset to that peak value.
#'
#' @seealso \code{\link{scale_to_reference_fun}}
#' @return A numeric vector of the same length as \code{x}.
#'
#' @examples
#' x <- c(
#'   0.685, 0.698, 0.717, 1.026, 1.216, 1.239, 1.123, 1.104, 0.999,
#'   0.81, 0.652, 0.633
#' )
#' x0 <- c(0.5, 0.5, 0.5, 0.7, 0.9, 1, 1, 1, 0.9, 0.7, 0.5, 0.5)
#' x_scaled1 <- scale_to_reference_peak_frequency(x, x0, cap_at_peak = TRUE)
#' print(x_scaled1)
#'
#' x_scaled2 <- scale_to_reference_peak_frequency(x, x0)
#' squash_into_low_high(x_scaled2, val_low = -Inf, val_high = max(x0))
#'
#' @export
scale_to_reference_peak_frequency <- function( # nolint
  x,
  x_ref,
  cap_at_peak = FALSE,
  na.rm = FALSE
) {
  n <- length(x_ref)
  stopifnot(length(x) == n)

  # Peak reference values
  rmax <- max(x_ref, na.rm = na.rm)

  # Number of peak reference values
  nmax <- max(1, n - sum(x_ref < rmax, na.rm = na.rm))

  # Un-scaled minimum value of peak number months
  ids <- order(x, decreasing = TRUE)[seq_len(nmax)]
  pmin <- min(x[ids], na.rm = na.rm)

  # Scale values to maintain the number of peak reference values
  res <- x * rmax / pmin

  # Make sure no values exceed peak
  if (cap_at_peak) {
    res <- pmin(res, rmax, na.rm = na.rm)
  }

  res
}


#' Scale values of a vector so that the result sums to 1
#'
#' @param x A numeric vector
#'
#' @seealso \code{\link{scale_to_reference_fun}}
#'
#' @examples
#' x <- c(1:6, 8)
#'
#' # Equivalent but more efficient than `scale_to_reference_fun`
#' scale_by_sum(x)
#' scale_to_reference_fun(x, 1, fun = sum)
#'
#' @export
scale_by_sum <- function(x) {
  tmp <- sum(x, na.rm = TRUE)
  if (tmp > 0 && is.finite(tmp)) {
    x / tmp
  } else {
    x
  }
}




#' Scale values so that the rounded values sum to 1
#'
#' @param x A one or two-dimensional numeric object with positive values:
#'   a vector, data frame, matrix, or array.
#'   A vector is treated as a 1-row matrix.
#' @param digits A integer value. The number of decimal places to round;
#'   a number between one and four.
#' @param icolumn_adjust An integer value. The column number of
#'   \code{x} that will be adjusted if rounded values have not converged
#'   to sum to 1 after \code{max_iter}.
#' @param max_iter An integer value. The maximum number of iterations.
#' @param verbose A logical value. Reports a warning if \code{TRUE} and
#'   \code{max_iter} is exceeded.
#'
#' @section Notes:
#' This function may fail due to integer division by zero.
#'
#' @section Notes:
#' This function may also not converge to round values so that they sum to 1,
#' particularly if too few \code{digits} are available to round. In that case,
#' attempt to increase \code{digits}. However, the return values should sum
#' to within \code{digits} of 1.
#'
#' @seealso \code{\link{scale_by_sum}}, \code{\link[base]{round}}
#'
#' @examples
#' x <- c(1:6, 8) / 10
#' scale_rounded_by_sum(x, digits = 1, icolumn_adjust = 7)
#' scale_rounded_by_sum(x, digits = 2, icolumn_adjust = 7)
#' scale_rounded_by_sum(x, digits = 4, icolumn_adjust = 7)
#'
#' x <- matrix(1:12, nrow = 4, ncol = 3)
#' scale_rounded_by_sum(x, digits = 1)
#' scale_rounded_by_sum(x, digits = 2)
#' scale_rounded_by_sum(x, digits = 4)
#'
#' x <- as.data.frame(x)
#' scale_rounded_by_sum(x, digits = 1)
#'
#' @export
scale_rounded_by_sum <- function(
  x,
  digits,
  icolumn_adjust = 1L,
  max_iter = max(4L, digits + 1L),
  verbose = FALSE
) {
  if (is.vector(x)) {
    x <- matrix(x, nrow = 1L)
  }

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  stopifnot(
    is.array(x) || is.matrix(x),
    length(dim(x)) == 2L,
    digits > 0 && digits <= 4L,
    all(apply(x, 1L, sum) > 0)
  )

  # Scale to sum to 1 with `digits` precision
  scale_sum <- as.integer(10^digits)
  sum1 <- scale_sum^2

  # Repeat scaling until sum is stable
  k <- 1

  repeat {
    tmpk <- array(as.integer(round(scale_sum * x)), dim = dim(x))

    sum_x <- apply(tmpk, 1L, sum)

    if (k >= max_iter && any(sum_x != sum1, na.rm = TRUE)) {
      if (verbose) {
        warning(
          "Rounded sum(s) not converged to 1 after ", k, " iterations: ",
          "column ", icolumn_adjust, " adjusted to fix sum to 1."
        )
      }
      tmpk[, icolumn_adjust] <- tmpk[, icolumn_adjust] + sum1 - sum_x
    }

    if (any(sum_x == 0)) {
      stop("Rounded sum(s) failed due to division by 0.")
    }

    x <- sweep(
      x = scale_sum * array(as.numeric(tmpk), dim = dim(tmpk)),
      MARGIN = 1L,
      STATS = sum_x,
      FUN = "%/%"
    )

    ok <- all(apply(x, 1L, sum) == scale_sum)

    if (k >= max_iter || ok) {
      break
    } else {
      k <- k + 1
    }
  }

  if (!ok) {
    warning("Rounded sum(s) not converged to 1.")
  }

  x / scale_sum
}
