context("Root-finding and optimizations")


#--- Tests
test_that("Roots:", {
  r1 <- uniroots(
    f = function(x) -1 + x^2,
    xlim = c(-10, 10),
    tol = rSW2_glovars[["tol"]],
    expected_nroots = 2
  )

  expect_type(r1, "list")

  expect_equal(
    has_uniroots(r1),
    c(TRUE, TRUE)
  )

  expect_equal(
    vapply(r1, function(x) x[["root"]], FUN.VALUE = NA_real_),
    c(-1, 1),
    tol = rSW2_glovars[["tol"]]
  )

  r2 <- uniroots(f = function(x) -1 + x^2, xlim = c(-10, -5))
  expect_type(r2, "list")
  expect_equal(
    has_uniroots(r2),
    FALSE
  )

  r3 <- uniroots(f = function(x) Inf, xlim = c(-10, -5), expected_nroots = 2)
  expect_type(r3, "list")
  expect_equal(
    has_uniroots(r3),
    FALSE
  )
})
