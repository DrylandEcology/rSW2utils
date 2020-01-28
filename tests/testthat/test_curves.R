context("Curve functions")

# Inputs

test_that("lines", {
  p1 <- c(1, 1)
  p2 <- c(-1, 5)

  lfun <- f_2pline(p1[1], p1[2], p2[1], p2[2])

  expect_equal(lfun(p1[1])[2], p1[2])
  expect_equal(lfun(p2[1])[2], p2[2])


  # Horizontal line
  p1 <- c(1, 1)
  p2 <- c(5, 1)

  lfun <- f_2pline(p1[1], p1[2], p2[1], p2[2])

  expect_equal(lfun(p1[1])[2], p1[2])
  expect_equal(lfun(p2[1])[2], p2[2])

  # Vertical line produces NAs
  p1 <- c(1, 1)
  p2 <- c(1, 2)

  lfun <- f_2pline(p1[1], p1[2], p2[1], p2[2])

  expect_equal(lfun(p1[1])[2], NA_real_)
  expect_equal(lfun(p2[1])[2], NA_real_)
})


test_that("circle", {
  thetas <- seq(0, 2 * pi, length = 200)

  cfun <- f_circle()

  cvals <- cfun(thetas)

  # x ^ 2 + y ^ 2 == 1
  expect_true(
    all(abs(apply(cvals ^ 2, 1, sum) - 1) < rSW2_glovars[["tol"]])
  )
})
