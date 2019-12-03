
#' Goodness of fit measures between observations and simulated values
#'
#' @param obs A numeric one- or two-dimensional object.
#' @param sim A numeric one- or two-dimensional object.
#' @param na.rm A logical value. Should (pairwise) missing values be removed?
#'
#' @section Details: \code{obs} and \code{sim} must have the same dimensions.
#'   If they are two-dimensional objects, then the GOFs are calculated for
#'   each column separately.
#'
#' @return A numeric vector of length corresponding to the number of columns
#'   (which is one in case of one-dimensional inputs.)
#'
#' @name gof
NULL

#' @describeIn gof Root Mean Square Error
#' @export
rmse <- function(obs, sim, na.rm = FALSE) {
  obs <- as.matrix(obs)
  sim <- as.matrix(sim)

  sqrt(colMeans((sim - obs) ^ 2, na.rm = na.rm))
}

#' @describeIn gof Mean Absolute Error
#' @export
mae <- function(obs, sim, na.rm = FALSE) {
  obs <- as.matrix(obs)
  sim <- as.matrix(sim)

  sqrt(colMeans(abs(sim - obs), na.rm = na.rm))
}


#' @describeIn gof Nash-Sutcliffe efficiency
#'
#' @references Nash, J. E. and Sutcliffe, J. V. (1970). River flow forecasting
#'   through conceptual models part I: A discussion of principles.
#'   Journal of Hydrology, 10:3, 282-290.
#'
#' @export
NSE <- function(obs, sim, na.rm = FALSE) {
  obs <- as.matrix(obs)
  sim <- as.matrix(sim)

  # Nominator
  n <- colSums((obs - sim) ^ 2, na.rm = na.rm)

  # Denominator
  temp <- colMeans(obs, na.rm = na.rm)
  temp <- sweep(obs, MARGIN = 2, STATS = temp, FUN = "-")
  d <- colSums(temp ^ 2, na.rm = na.rm)

  # NSE
  res <- rep(-Inf, ncol(obs))

  is_denom_nonzero <- abs(d) > sqrt(.Machine$double.eps)
  res[is_denom_nonzero] <- 1 - n[is_denom_nonzero] / d[is_denom_nonzero]

  res
}
