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


#' Extend range by factor
#'
#' @param x A numeric vector
#' @param fact A numeric value
#' @param na.rm A logical value
#'
#' @export
extend_range <- function(x, fact = 1, na.rm = FALSE) {
  x + (fact - 1) * (x - mean(x, na.rm = na.rm))
}


#' Stretch values
#'
#' Values above the mean of \code{x} are made larger and
#' values below the mean are made smaller - each by
#' \code{lambda * dist(x, mean(x))}.
#'
#' @param x A numeric vector.
#' @param lambda A numeric value. The stretching factor applied to \code{x}.
#'
#' @return A numeric vector of the size of \code{x}.
#' @export
stretch_values <- function(x, lambda = 0) {
  (1 + lambda) * x - lambda * mean(x)
}




#' Find the \code{k}-largest/smallest values (and apply a function to these
#' values)
#'
#' @param x A numeric vector
#' @param largest A logical value. See return value.
#' @param fun A function which requires one argument or \code{"index"}.
#'   \code{fun} will be applied to the \code{k}-largest/smallest values of
#'   \code{x}.
#' @param k An integer value. The \code{k}-largest/smallest value(s) of \code{x}
#'   will be used. The largest/smallest value will be used if 0 or negative.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
#' @param \dots Optional arguments to be passed to \code{fun}
#'
#' @return A vector of length \code{k}, \itemize{
#'   \item if \code{is.null(fun)}, then a vector with the \code{k}-largest
#'     (if \code{largest = TRUE}) or \code{k}-smallest
#'     (if \code{largest = FALSE}) values of \code{x};
#'   \item if \code{fun = "index"}, then a vector with indices of the
#'     \code{k}-largest/smallest values (NOTE: this is truncated to the
#'     \code{k}-first indices!). } Otherwise, the result of applying \code{fun}
#'     to the \code{k}-largest/smallest values.
#'
#' @export
fun_kLargest <- function(x, largest = TRUE, fun = NULL, k = 10L,
  na.rm = FALSE, ...) {

  res <- if (na.rm) {
    stats::na.exclude(x)
  } else {
    x
  }

  # Determine k-largest/smallest values
  res <- sort.int(res, decreasing = largest, na.last = !na.rm,
    method = if (getRversion() >= "3.3.0") "radix" else "quick")
  res <- res[seq_len(max(1L, min(length(res), as.integer(k))))]

  # Calculate return values
  if (is.null(fun)) {
    res
  } else if (identical(fun, "index")) {
    which(x %in% res)[seq_len(k)]
  } else {
    fun(res, ...)
  }
}




#' Recursive comparisons for equality which also works for nested lists
#'
#' @param x1 A R object
#' @param x2 A R object
#'
#' @seealso \code{\link{all.equal}}
#'
#' @return \itemize{
#'  \item If both \code{x1} and \code{x2} are lists, then
#'    \code{all_equal_recursively} is called recursively on mutually shared
#'    names if names exists and on each element otherwise, and the output
#'    is a list from the return value of each recursive call.
#'  \item Otherwise, the function \code{\link{all.equal}} is called. If the
#'    result is \code{TRUE}, then \code{NA} is returned. If the result is
#'    \code{FALSE}, then a list with three elements is returned with \describe{
#'    \item{eq}{the result of the call to \code{\link{all.equal}}}
#'    \item{x1}{The object \code{x1}}
#'    \item{x2}{The object \code{x2}}
#'  }}
#'
#' @examples
#'  ## expected result: NA
#'  all_equal_recursively(1L, 1L)
#'
#'  ## expected result: list(eq = "Mean relative difference: 1", x1 = 1, x2 = 2)
#'  all_equal_recursively(1, 2)
#'
#   ## expected result: first comparison returns NA; second shows a difference
#'  all_equal_recursively(list(1, 2), list(1, 3))
#'  ## expected result: comparison for elements a and b return NA; comparison
#'  ## for element c shows a difference
#'  all_equal_recursively(list(a = 1, b = 2), list(b = 2, c = 0, a = 1))
#' @export
all_equal_recursively <- function(x1, x2) {
  if (is.list(x1) && is.list(x2)) {
    dims <- if (!is.null(names(x1)) && !is.null(names(x2))) {
      unique(c(names(x1), names(x2)))
    } else {
      seq_len(min(length(x1), length(x2)))
    }

    # as of R v3.4.1 'Recall' doesn't work as argument to apply-type calls
    res <- lapply(dims,
      function(k) all_equal_recursively(x1 = x1[[k]], x2 = x2[[k]])
    )
    names(res) <- dims
    res

  } else {
    eq <- all.equal(x1, x2)

    if (isTRUE(eq)) {
      NA
    } else {
      list(eq = eq, x1 = x1, x2 = x2)
    }
  }
}
