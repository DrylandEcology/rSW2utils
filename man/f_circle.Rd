% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curves.R
\name{f_circle}
\alias{f_circle}
\title{Circle function with angle argument \code{theta}}
\usage{
f_circle(x0 = 0, y0 = 0, r = 1, theta0 = 0, dir = 1)
}
\arguments{
\item{x0}{A numeric value. The x-axis value of the circle center.}

\item{y0}{A numeric value. The y-axis value of the circle center.}

\item{r}{A numeric value. The circle radius.}

\item{theta0}{A numeric value. The (anti-clockwise) angle/rotation of the
starting point of the circle.}

\item{dir}{A numeric value, interpreted as either
\code{-1}, i.e., clockwise, or as \code{1}, i.e., anti-clockwise
direction.}
}
\description{
Circle function with angle argument \code{theta} such that
\code{sign(dir) * theta + theta0 = 0} corresponds to \code{c(x0 + r, y0)}
}
\examples{
thetas <- seq(0, 2 * pi, length = 200)
# circle with radius 1 and center at 0/0
circle1 <- f_circle()
plot(circle1(thetas), asp = 1, type = "l")
points(0, 0, pch = 4)
# circle with radius 0.1 and center at 0.3/0.3
lines(f_circle(0.3, 0.3, 0.1)(thetas))
# circle with radius 0.1 and center at -0.3/0.3
lines(f_circle(-0.3, 0.3, 0.1)(thetas))
# semi-circle with radius 0.5 and center at 0/0
lines(f_circle(0, 0, 0.5, dir = -1)(seq(0, pi, length = 200)))

}
