context("Curve functions")

# Inputs

test_that("lines", {
  p1 <- c(1, 1)
  p2 <- c(-1, 5)

  lfun <- f_2pline(p1[[1L]], p1[[2L]], p2[[1L]], p2[[2L]])

  expect_identical(lfun(p1[[1L]])[[2L]], p1[[2L]])
  expect_identical(lfun(p2[[1L]])[[2L]], p2[[2L]])


  # Horizontal line
  p1 <- c(1, 1)
  p2 <- c(5, 1)

  lfun <- f_2pline(p1[[1L]], p1[[2L]], p2[[1L]], p2[[2L]])

  expect_identical(lfun(p1[[1L]])[[2L]], p1[[2L]])
  expect_identical(lfun(p2[[1L]])[[2L]], p2[[2L]])

  # Vertical line produces NAs
  p1 <- c(1, 1)
  p2 <- c(1, 2)

  lfun <- f_2pline(p1[[1L]], p1[[2L]], p2[[1L]], p2[[2L]])

  expect_identical(lfun(p1[[1L]])[[2L]], NA_real_)
  expect_identical(lfun(p2[[1L]])[[2L]], NA_real_)
})


test_that("circle", {
  thetas <- seq(0, 2 * pi, length = 200)
  r <- c(1, 0.5, 2)

  for (k in seq_along(r)) {
    cfun <- f_circle(r = r[k])

    cvals <- cfun(thetas)

    # (x ^ 2 + y ^ 2) / r ^ 2 == 1 #nolint
    cvals <- cvals / r[k]
    expect_true(
      all(abs(apply(cvals^2, 1, sum) - 1) < rSW2_glovars[["tol"]])
    )
  }
})



test_that("ellipse", {
  thetas <- seq(0, 2 * pi, length = 200)
  r <- list(
    a = c(1, 0.5, 2),
    b = c(1, 2, 0.5)
  )

  for (k in seq_along(r)) {
    cfun <- f_ellipse(a = r[["a"]][k], b = r[["b"]][k])

    cvals <- cfun(thetas)

    # x ^ 2 / a ^ 2 + y ^ 2 / b ^ 2 == 1 #nolint
    cvals[, 1] <- cvals[, 1] / r[["a"]][k]
    cvals[, 2] <- cvals[, 2] / r[["b"]][k]
    expect_true(
      all(abs(apply(cvals^2, 1, sum) - 1) < rSW2_glovars[["tol"]])
    )
  }
})
