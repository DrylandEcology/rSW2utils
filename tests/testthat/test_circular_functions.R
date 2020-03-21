context("Circular functions")


#--- Tests
test_that("Circular mean", {
  x <- 1:3
  expect_equal(circ_mean(x, int = 12, type = "minusPiPlusPi"), 2)
  expect_equal(circ_mean(x, int = 12, type = "ZeroPlus2Pi"), 2)

  x <- (-2):0
  expect_equal(circ_mean(x, int = 12, type = "minusPiPlusPi"), -1)
  expect_equal(circ_mean(x, int = 12, type = "ZeroPlus2Pi"), 11)

  x <- (-5):5
  expect_equal(circ_mean(x, int = 12, type = "minusPiPlusPi"), 0)
  expect_equal(circ_mean(x, int = 12, type = "ZeroPlus2Pi"), 12)

  x <- (-5):8
  expect_equal(circ_mean(x, int = 12, type = "minusPiPlusPi"), -4.5)
  expect_equal(circ_mean(x, int = 12, type = "ZeroPlus2Pi"), 7.5)

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
  x <- seq_len(12)
  w <- c(1, rep(0, 7), 1, 1, 2, 1)

  # stats::weighted.mean(x, w) ## expected 9
  expect_equal(circ_mean_weighted(x, w, int = 12, type = "minusPiPlusPi"), -1)
  expect_equal(circ_mean_weighted(x, w, int = 12, type = "ZeroPlus2Pi"), 11)
})


test_that("Circular subtraction", {
  # Days of year
  expect_equal(circ_minus(260, 240, int = 365), 20)
  expect_equal(circ_minus(240, 260, int = 365), -20)
  expect_equal(circ_minus(240, 260, int = 365, type = "ZeroPlus2Pi"), 345)
  expect_equal(circ_minus(10, 360, int = 365), 15)
  expect_equal(circ_minus(360, 10, int = 365), -15)
  expect_equal(circ_minus(360, 10, int = 365, type = "ZeroPlus2Pi"), 350)
  expect_equal(circ_minus(0, 360, int = 365), 5)
  expect_equal(circ_minus(360, 0, int = 365), -5)
  expect_equal(circ_minus(360, 0, int = 365, type = "ZeroPlus2Pi"), 360)

  # Matrix examples
  x <- matrix(c(260, 240, 10, 360, 0, 360), nrow = 3, ncol = 2)
  y <- matrix(c(240, 260, 360, 10, 360, 0), nrow = 3, ncol = 2)
  tmp <- structure(c(20, -20, 15, -15, 5, -5), .Dim = 3:2)

  expect_equal(circ_minus(x, y, int = 365), tmp)

  y[1, 1] <- tmp[1, 1] <- NA
  expect_equal(circ_minus(x, y, int = 365), tmp)
})


test_that("Circular addition", {
  # Matrix examples: day of year
  x <- matrix(c(260, 240, 10, 360, 0, 360), nrow = 3, ncol = 2)
  y <- matrix(c(240, 260, 360, 10, 360, 0), nrow = 3, ncol = 2)
  tmp1 <- structure(c(135, 135, 5, 5, -5, -5), .Dim = 3:2)
  tmp2 <- structure(c(135, 135, 5, 5, 360, 360), .Dim = 3:2)

  expect_equal(circ_add(x, y, int = 365), tmp1)
  expect_equal(circ_add(x, y, int = 365, type = "ZeroPlus2Pi"), tmp2)

  # Circular addition and subtraction
  r1 <- circ_add(circ_minus(x, y, int = 365), y, int = 365)
  r2 <- circ_minus(circ_add(x, y, int = 365), y, int = 365)
  expect_equal(r1, r2)
})


test_that("Circular sequences", {
  expect_equal(circ_seq(5, 8, int = 12), c(5, 6, 7, 8))
  expect_equal(circ_seq(-7, 8, int = 12), c(5, 6, 7, 8))
  expect_equal(circ_seq(-2, 3, int = 12), c(10, 11, 12, 1, 2, 3))
  expect_equal(circ_seq(-2, 3, int = 12, by = 2), c(10, 12, 2))
  expect_equal(circ_seq(-2, 3, int = 12, length.out = 3), c(10, 0.5, 3))

  expect_equal(circ_seq(-2, -2, int = 12), 10)
  expect_equal(circ_seq(10, 10, int = 12), 10)
  expect_equal(circ_seq(-2, -2, int = 12, by = 2), 10)
  expect_equal(circ_seq(-2, -2, int = 12, by = 0), 10)
  expect_equal(circ_seq(-2, -2, int = 12, length.out = 3), rep(10, 3))

  expect_error(circ_seq(5, 8, int = 12, by = c(1, 3)))
  expect_error(circ_seq(5, 8, int = 12, by = 0))
  expect_error(circ_seq(5, 8, int = 12, by = -3))
  expect_error(circ_seq(5, 8, int = 12, by = 1e-10))
})
