context("Bounded values")

#---TESTS
test_that("replace_NAs_with_val", {
  # tests if length is one
  x0 <- 1
  expect_identical(replace_NAs_with_val(x0, val_replace = NULL), x0)
  expect_identical(replace_NAs_with_val(x0, val_replace = NA), x0)
  expect_identical(replace_NAs_with_val(x0, val_replace = 2), x0)

  x0 <- NA
  tmp <- NA
  expect_identical(replace_NAs_with_val(x0, val_replace = tmp), tmp)
  tmp <- 2
  expect_identical(replace_NAs_with_val(x0, val_replace = tmp), tmp)

  # tests if length is larger than one
  n <- 3
  x0 <- rep(NA, n)
  tmp <- rep(NA, n)
  expect_identical(replace_NAs_with_val(x0, val_replace = tmp[[1L]]), tmp)
  tmp <- rep(2, n)
  expect_identical(replace_NAs_with_val(x0, val_replace = tmp[[1L]]), tmp)
})


test_that("cut0Inf", {
  # tests if length is one
  x0 <- 1
  tmp <- NA_real_
  expect_identical(cut0Inf(x0, val = tmp), x0)
  tmp <- -2
  expect_identical(cut0Inf(x0, val = tmp), x0)

  x0 <- -1
  tmp <- NA_real_
  expect_identical(cut0Inf(x0, val = tmp), tmp)
  tmp <- -2
  expect_identical(cut0Inf(x0, val = tmp), tmp)

  # tests if length is larger than one
  n <- 3
  x0 <- rep(-1, n)
  tmp <- rep(NA_real_, n)
  expect_identical(cut0Inf(x0, val = tmp[[1L]]), tmp)
  tmp <- rep(2, n)
  expect_identical(cut0Inf(x0, val = tmp[[1L]]), tmp)

  x0 <- c(1, NA, -1, NA, 1)
  tmp <- c(1, NA, 0, NA, 1)
  expect_identical(cut0Inf(x0, val = tmp[[3L]]), tmp)
})


test_that("finite01", {
  # tests if length is one
  expect_identical(finite01(NA), 0)
  expect_identical(finite01(-1), 0)
  expect_identical(finite01(0 - rSW2_glovars[["tol"]]), 0)
  expect_identical(finite01(tmp <- 0 + rSW2_glovars[["tol"]]), tmp)
  expect_identical(finite01(tmp <- 1 - rSW2_glovars[["tol"]]), tmp)
  expect_identical(finite01(1 + rSW2_glovars[["tol"]]), 1)
  expect_identical(finite01(10), 1)
  expect_identical(finite01(0.5), 0.5)

  # tests if length is larger than one
  expect_identical(
    finite01(c(10, 1, NA, -1, NA, 1, 0, 0.5)),
    c(1, 1, 0, 0, 0, 1, 0, 0.5)
  )

  expect_identical(
    finite01(c(10, 1, NA, -1, NA, 1, 0, 0.5), val_high_replace = NA),
    c(NA, 1, 0, 0, 0, 1, 0, 0.5)
  )

  expect_identical(
    finite01(
      c(10, 1, NA, -1, NA, 1, 0, 0.5),
      val_low_replace = -3,
      val_high_replace = 2
    ),
    c(2, 1, -3, -3, -3, 1, 0, 0.5)
  )
})

test_that("squash_into_low_high", {
  x0 <- c(10, 1, NA, -1, NA, 1, 0, 0.5)

  expect_identical(
    squash_into_low_high(x0),
    c(1, 1, NA, 0, NA, 1, 0, 0.5)
  )
  expect_identical(
    squash_into_low_high(x0, val_low = NULL),
    c(1, 1, NA, -1, NA, 1, 0, 0.5)
  )
  expect_identical(
    squash_into_low_high(x0, val_high = NULL),
    c(10, 1, NA, 0, NA, 1, 0, 0.5)
  )
  expect_identical(
    squash_into_low_high(x0, val_low = NULL, val_high = NULL),
    x0
  )

  expect_identical(
    squash_into_low_high(
      x0,
      val_low = 0, val_low_replace = -3,
      val_high = 1, val_high_replace = 2
    ),
    c(2, 1, NA, -3, NA, 1, 0, 0.5)
  )
})
