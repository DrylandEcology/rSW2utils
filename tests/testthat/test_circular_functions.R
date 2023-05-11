
#--- Tests
test_that("Circular mean", {
  x <- 1:3
  tol <- sqrt(.Machine[["double.eps"]])

  expect_equal(
    circ_mean(x, int = 12, type = "minusPiPlusPi"),
    2,
    tolerance = tol
  )
  expect_identical(circ_mean(x, int = 12, type = "ZeroPlus2Pi"), 2)

  x <- (-2):0
  expect_equal(
    circ_mean(x, int = 12, type = "minusPiPlusPi"),
    -1,
    tolerance = tol
  )
  expect_identical(circ_mean(x, int = 12, type = "ZeroPlus2Pi"), 11)

  x <- (-5):5
  expect_identical(circ_mean(x, int = 12, type = "minusPiPlusPi"), 0)
  expect_identical(circ_mean(x, int = 12, type = "ZeroPlus2Pi"), 12)

  x <- (-5):8
  expect_equal(
    circ_mean(x, int = 12, type = "minusPiPlusPi"),
    -4.5,
    tolerance = tol
  )
  expect_identical(circ_mean(x, int = 12, type = "ZeroPlus2Pi"), 7.5)

  # Check that values are
  #   * between -int / 2 and int / 2 for type == "minusPiPlusPi"
  #   * between 0 and int for type == "ZeroPlus2Pi"
  N <- 10
  cycle <- 12L
  hc <- cycle / 2L
  tmp0 <- seq(-hc, cycle + hc)
  tmp <- seq(-hc, hc)

  set.seed(123)
  tests <- c(
    lapply(tmp0, function(x) x + runif(N, min = -hc, max = hc)),
    lapply(tmp0, function(x) x + tmp)
  )

  for (k in seq_along(tests)) {
    x <- circ_mean(tests[[k]], int = cycle, type = "ZeroPlus2Pi")
    expect_gte(x, 0)
    expect_lte(x, cycle)

    x <- circ_mean(tests[[k]], int = cycle, type = "minusPiPlusPi")
    expect_gte(x, -hc)
    expect_lte(x, hc)
  }
})


test_that("Circular weighted mean", {
  tol <- sqrt(.Machine[["double.eps"]])

  x <- seq_len(12)
  w <- c(1, rep(0, 7), 1, 1, 2, 1)

  # stats::weighted.mean(x, w) ## expected 9 #nolint
  expect_equal(
    circ_mean_weighted(x, w, int = 12, type = "minusPiPlusPi"),
    -1,
    tolerance = tol
  )

  expect_identical(
    circ_mean_weighted(x, w, int = 12, type = "ZeroPlus2Pi"),
    11
  )
})


test_that("Circular subtraction", {
  tol <- sqrt(.Machine[["double.eps"]])

  # Days of year
  expect_identical(circ_minus(260, 240, int = 365), 20)
  expect_identical(circ_minus(240, 260, int = 365), -20)
  expect_identical(circ_minus(240, 260, int = 365, type = "ZeroPlus2Pi"), 345)
  expect_equal(circ_minus(10, 360, int = 365), 15, tolerance = tol)
  expect_equal(circ_minus(360, 10, int = 365), -15, tolerance = tol)
  expect_identical(circ_minus(360, 10, int = 365, type = "ZeroPlus2Pi"), 350)
  expect_identical(circ_minus(0, 360, int = 365), 5)
  expect_identical(circ_minus(360, 0, int = 365), -5)
  expect_identical(circ_minus(360, 0, int = 365, type = "ZeroPlus2Pi"), 360)

  # Matrix examples
  x <- matrix(c(260, 240, 10, 360, 0, 360), nrow = 3, ncol = 2)
  y <- matrix(c(240, 260, 360, 10, 360, 0), nrow = 3, ncol = 2)
  tmp <- structure(c(20, -20, 15, -15, 5, -5), .Dim = 3:2)

  expect_equal(
    circ_minus(x, y, int = 365),
    tmp,
    tolerance = tol
  )

  y[1, 1] <- tmp[1, 1] <- NA
  expect_equal(
    circ_minus(x, y, int = 365),
    tmp,
    tolerance = tol
  )
})


test_that("Circular addition", {
  tol <- sqrt(.Machine[["double.eps"]])

  # Matrix examples: day of year
  x <- matrix(c(260, 240, 10, 360, 0, 360), nrow = 3, ncol = 2)
  y <- matrix(c(240, 260, 360, 10, 360, 0), nrow = 3, ncol = 2)

  expect_equal(
    circ_add(x, y, int = 365),
    structure(c(135, 135, 5, 5, -5, -5), .Dim = 3:2),
    tolerance = tol
  )

  expect_equal(
    circ_add(x, y, int = 365, type = "ZeroPlus2Pi"),
    structure(c(135, 135, 5, 5, 360, 360), .Dim = 3:2),
    tolerance = tol
  )

  # Circular addition and subtraction
  expect_equal(
    circ_add(circ_minus(x, y, int = 365), y, int = 365),
    circ_minus(circ_add(x, y, int = 365), y, int = 365),
    tolerance = tol
  )
})


test_that("Circular sequences", {
  expect_identical(circ_seq(5, 8, int = 12), c(5, 6, 7, 8))
  expect_identical(circ_seq(-7, 8, int = 12), c(5, 6, 7, 8))
  expect_identical(circ_seq(-2, 3, int = 12), c(10, 11, 12, 1, 2, 3))
  expect_identical(circ_seq(-2, 3, int = 12, by = 2), c(10, 12, 2))
  expect_identical(circ_seq(-2, 3, int = 12, length.out = 3), c(10, 0.5, 3))

  expect_identical(circ_seq(-2, -2, int = 12), 10)
  expect_identical(circ_seq(10, 10, int = 12), 10)
  expect_identical(circ_seq(-2, -2, int = 12, by = 2), 10)
  expect_identical(circ_seq(-2, -2, int = 12, by = 0), 10)
  expect_identical(circ_seq(-2, -2, int = 12, length.out = 3), rep(10, 3))

  expect_error(circ_seq(5, 8, int = 12, by = c(1, 3)))
  expect_error(circ_seq(5, 8, int = 12, by = 0))
  expect_error(circ_seq(5, 8, int = 12, by = -3))
  expect_error(circ_seq(5, 8, int = 12, by = 1e-10))
})
