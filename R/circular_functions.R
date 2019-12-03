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


#' Functions for circular descriptive statistics
#'
#' @param x A numeric vector or a matrix. If a data.frame is supplied, then
#'   \code{x} is coerced to a matrix.
#' @param int A numeric value. The number of units of \code{x} in a full circle,
#'   e.g., for unit days: \code{int = 365}; for unit months: \code{int = 12}.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
#' @param type A character string. If \code{type == "minusPiPlusPi"},
#'   then the resulting value lies between \code{-int / 2} and \code{int / 2}.
#'   If \code{type == "ZeroPlus2Pi"},
#'   then the resulting value lies between \code{0} and \code{int}.
#'
#'
#' @return A numeric value or \code{NA}.
#'
#' @seealso \code{\link[circular]{mean.circular}},
#'   \code{\link[circular]{range.circular}},
#'   \code{\link[circular]{sd.circular}}
#'
#' @aliases circ_mean circ_range circ_sd
#' @name circular
NULL

ZeroPlus2Pi <- function(x, int) {
  rSW2_glovars[["tol"]] + (x - rSW2_glovars[["tol"]]) %% int
}

get_circular_type <- function(x, circ, int,
  type = c("minusPiPlusPi", "ZeroPlus2Pi")) {
  type <- match.arg(type)

  if (type == "minusPiPlusPi") {
    x <- circular::minusPiPlusPi(x)
  }

  res <- as.numeric(x / circ)

  if (type == "ZeroPlus2Pi") {
    res <- ZeroPlus2Pi(res, int)
  }

  res
}

#' @rdname circular
#' @examples
#' x <- 1:3
#' circ_mean(x, int = 12, type = "minusPiPlusPi") ## expected 2
#' circ_mean(x, int = 12, type = "ZeroPlus2Pi") ## expected 2
#' x <- (-2):0
#' circ_mean(x, int = 12, type = "minusPiPlusPi") ## expected -1
#' circ_mean(x, int = 12, type = "ZeroPlus2Pi") ## expected 11
#' x <- (-5):5
#' circ_mean(x, int = 12, type = "minusPiPlusPi") ## expected 0
#' circ_mean(x, int = 12, type = "ZeroPlus2Pi") ## expected 12
#' x <- (-5):8
#' circ_mean(x, int = 12, type = "minusPiPlusPi") ## expected -4.5
#' circ_mean(x, int = 12, type = "ZeroPlus2Pi") ## expected 7.5
#'
#' @export
circ_mean <- function(x, int, type = c("minusPiPlusPi", "ZeroPlus2Pi"),
  na.rm = FALSE) {
  type <- match.arg(type)

  if (!all(is.na(x)) && requireNamespace("circular", quietly = TRUE)) {
    circ <- 2 * pi / int
    x_circ <- circular::circular(x * circ,
      type = "angles",
      units = "radians",
      rotation = "clock",
      modulo = "2pi")

    res_circ <- circular::mean.circular(x_circ, na.rm = na.rm)

    get_circular_type(res_circ, circ, int, type)

  } else {
    NA
  }
}

#' @rdname circular
#'
#' @export
circ_range <- function(x, int, na.rm = FALSE) {
  if (!all(is.na(x)) && requireNamespace("circular", quietly = TRUE)) {
    circ <- 2 * pi / int
    x_circ <- circular::circular(x * circ,
      type = "angles",
      units = "radians",
      rotation = "clock",
      modulo = "2pi")

    x_int <- range(x_circ, na.rm = na.rm) / circ
    as.numeric(x_int)

  } else {
    NA
  }
}

#' @rdname circular
#'
#' @export
circ_sd <- function(x, int, na.rm = FALSE) {
  if (length(x) - sum(is.na(x)) > 1 && requireNamespace("circular",
    quietly = TRUE)) {

    if (sd(x, na.rm = TRUE) > 0) {
      circ <- 2 * pi / int
      x_circ <- circular::circular(x * circ,
        type = "angles",
        units = "radians",
        rotation = "clock",
        modulo = "2pi")

      x_int <- circular::sd.circular(x_circ, na.rm = na.rm) / circ
      as.numeric(x_int)

    } else {
      0
    }
  } else {
    NA
  }
}


#' Calculate the circular subtraction \var{x - y}
#'
#' @param x A numeric vector or array.
#' @param y A numeric vector or array.
#' @inheritParams circular
#'
#' @examples
#' # Days of year
#' circ_minus(260, 240, int = 365) ## expected: +20
#' circ_minus(240, 260, int = 365) ## expected: -20
#' circ_minus(240, 260, int = 365, type = "ZeroPlus2Pi") ## expected: 345
#' circ_minus(10, 360, int = 365) ## expected: +15
#' circ_minus(360, 10, int = 365) ## expected: -15
#' circ_minus(360, 10, int = 365, type = "ZeroPlus2Pi") ## expected: 350
#' circ_minus(0, 360, int = 365) ## expected: +5
#' circ_minus(360, 0, int = 365) ## expected: -5
#' circ_minus(360, 0, int = 365, type = "ZeroPlus2Pi") ## expected: 360
#'
#' # Matrix examples
#' x <- matrix(c(260, 240, 10, 360, 0, 360), nrow = 3, ncol = 2)
#' y <- matrix(c(240, 260, 360, 10, 360, 0), nrow = 3, ncol = 2)
#' circ_minus(x, y, int = 365)
#' y2 <- y
#' y2[1, 1] <- NA
#' circ_minus(y2, x, int = 365)
#'
#' @export
circ_minus <- function(x, y, int, type = c("minusPiPlusPi", "ZeroPlus2Pi")) {
  stopifnot(all(dim(x) == dim(y)))

  type <- match.arg(type)

  if (requireNamespace("circular", quietly = TRUE)) {
    circ <- 2 * pi / int

    d_circ <- circular::circular((x - y) * circ,
      type = "angles",
      units = "radians",
      rotation = "clock",
      modulo = "asis")

    res <- get_circular_type(d_circ, circ, int, type)

  } else {
    res <- rep(NA, length(x))
  }

  if (is.array(x)) {
    array(res, dim = dim(x), dimnames = dimnames(x))
  } else {
    res
  }
}


#' Calculate the circular addition \var{x + y}
#'
#' @inheritParams circ_minus
#' @inheritParams circular
#'
#' @examples
#' # Matrix examples: day of year
#' x <- matrix(c(260, 240, 10, 360, 0, 360), nrow = 3, ncol = 2)
#' y <- matrix(c(240, 260, 360, 10, 360, 0), nrow = 3, ncol = 2)
#' circ_add(x, y, int = 365)
#' circ_add(x, y, int = 365, type = "ZeroPlus2Pi")
#'
#' # Circular addition and subtraction
#' r1 <- circ_add(circ_minus(x, y, int = 365), y, int = 365)
#' r2 <- circ_minus(circ_add(x, y, int = 365), y, int = 365)
#' all.equal(r1, r2)
#'
#' @export
circ_add <- function(x, y, int, type = c("minusPiPlusPi", "ZeroPlus2Pi")) {
  stopifnot(all(dim(x) == dim(y)))

  type <- match.arg(type)

  if (requireNamespace("circular", quietly = TRUE)) {
    circ <- 2 * pi / int

    d_circ <- circular::circular((x + y) * circ,
      type = "angles",
      units = "radians",
      rotation = "clock",
      modulo = "asis")

    res <- get_circular_type(d_circ, circ, int, type)

  } else {
    res <- rep(NA, length(x))
  }

  if (is.array(x)) {
    array(res, dim = dim(x), dimnames = dimnames(x))
  } else {
    res
  }
}




#' Sequence generation for circular data
#'
#' @inheritParams base::seq
#' @inheritParams circular
#'
#' @seealso \code{\link{seq}}
#'
#' @examples
#' circ_seq(5, 8, int = 12)          ## expected c(5, 6, 7, 8)
#' circ_seq(-7, 8, int = 12)         ## expected c(5, 6, 7, 8)
#' circ_seq(-2, 3, int = 12)         ## expected c(10, 11, 12, 1, 2, 3)
#' circ_seq(-2, 3, int = 12, by = 2) ## expected c(10, 12, 2)
#' circ_seq(-2, 3, int = 12, length.out = 3) ## expected c(10, 0.5, 3)
#'
#' @export
circ_seq <- function(from, to, int, by, length.out = NULL) {

  # Modulo `p`
  p <- int
  from <- from %% p
  to <- to %% p

  # Code from `seq.default`: start
  is.logint <- function(.) (is.integer(.) || is.logical(.)) && !is.object(.)
  int <- is.logint(from) && is.logint(to)
  # Code from `seq.default`: end

  del <- circ_minus(to, from, int = p, type = "ZeroPlus2Pi")

  # Code from `seq.default`: start
  if (del == 0 && to == 0) {
    return(to)
  }
  # Code from `seq.default`: end

  # Determine `by`
  if (missing(by) || is.null(by)) {
    if (is.null(length.out)) {
      by <- 1L
    } else {
      by <- del / (length.out - 1)
    }
  }

  # Code from `seq.default`: start
  if (length(by) != 1L) {
    stop("'by' must be of length 1")
  }

  if (!is.logint(by)) {
    int <- FALSE

  } else if (!int) {
    storage.mode(by) <- "double"
  }

  n <- del / by

  if (!is.finite(n)) {
    if (!is.na(by) && by == 0 && del == 0) {
      return(from)
    }
    stop("invalid '(to - from)/by'")
  }

  if (n < 0L) {
    stop("wrong sign in 'by' argument")
  }

  if (n > .Machine$integer.max) {
    stop("'by' argument is much too small")
  }

  dd <- abs(del) / max(abs(to), abs(from))
  if (dd < 100 * .Machine$double.eps) {
    return(from)
  }
  # Code from `seq.default`: end

  # Code based on `seq.default`: start
  n <- if (int) {
    as.integer(n)
  } else {
    as.integer(n + rSW2_glovars[["tol"]])
  }

  res <- from + (0L:n) * by
  # Code based on `seq.default`: end


  # Modulo `p` type "ZeroPlus2Pi"
  ZeroPlus2Pi(res, p)
}
