tol <- sqrt(.Machine[["double.eps"]])

#--- TESTS
test_that("Sequence positions", {
  #--- Run sequences ------
  # Test data
  td <- list(
    list(x = c(0, 1, 0, 0, 0), start = 2L, seq = list(2L)),
    list(x = c(0, 1, 1, 0, 0), start = 2L, seq = list(2L:3L)),
    list(x = c(1, 1, 0, 0), start = 1L, seq = list(1L:2L)),
    list(x = c(1, 1, 1, 1), start = 1L, seq = list(1L:4L)),
    list(x = c(1, 0, 1, 1), start = c(1L, 3L), seq = list(1L, 3L:4L)),
    list(x = c(1, 0, 0, 1), start = c(1L, 4L), seq = list(1L, 4L)),
    list(x = c(1.5, 0, 15, 3.5), start = c(1L, 3L), seq = list(1L, 3L:4L)),
    list(x = c(0, 0), start = integer(), seq = list())
  )

  for (k in seq_along(td)) {
    expect_identical(calc_runs(td[[k]][["x"]]), td[[k]][["seq"]])
    expect_identical(calc_starts(td[[k]][["x"]]), td[[k]][["start"]])
  }

  #--- Run maximum durations ------
  expect_identical(max_duration(0, target_val = 1), 0L)
  x1 <- c(rep(3, 3), rep(10, 10), 4, rep(10, 20), rep(3, 6))
  expect_identical(max_duration(x1, 10, FALSE), 20L)
  expect_identical(unname(max_duration(x1, 10, TRUE)), c(20, 15, 34))
  x2 <- c(x1, rep(3, 3), rep(10, 20))
  expect_identical(max_duration(x2, 10, FALSE), 20L)
  expect_identical(unname(max_duration(x2, 10, TRUE)), c(20, 15, 34))
})


test_that("Moving window", {
  x <- list(
    rep(1, 365),
    runif(100)
  )

  funs <- c("median", "mean", "sum", "sd")

  for (tmp in x) {
    for (k in c(1, 3, 7, 31)) {
      # Moving median
      fun <- median
      mf <- moving_function(x = tmp, k = k, win_fun = fun, na.rm = FALSE)
      isnotna <- !is.na(mf)

      expect_identical(
        as.vector(stats::runmed(x = tmp, k = k))[isnotna],
        mf[isnotna]
      )

      # Moving general functions
      for (fun in funs) {
        f <- match.fun(fun)

        mf <- moving_function(x = tmp, k = k, win_fun = f, na.rm = FALSE)
        isnotna <- !is.na(mf)

        expect_equal(
          as.vector(stats::na.omit(apply(stats::embed(tmp, k), 1, f))),
          mf[isnotna],
          tolerance = tol
        )

        # Check na.rm
        mf2 <- moving_function(x = tmp, k = k, win_fun = f, na.rm = TRUE)
        if (!(fun == "sd" && k == 1)) {
          expect_identical(sum(is.na(mf2)), 0L)
        }
        expect_identical(mf[isnotna], mf2[isnotna])

        # Check circular
        mf3 <- moving_function(
          x = tmp,
          k = k,
          win_fun = f,
          na.rm = FALSE,
          circular = TRUE
        )
        if (!(fun == "sd" && k == 1)) {
          expect_identical(sum(is.na(mf3)), 0L)
        }
        expect_identical(mf[isnotna], mf3[isnotna])
      }
    }
  }
})


test_that("Counting peaks", {
  expect_identical(count_peaks(c(0, 1, 0, 1, 0)), 2L)
  expect_identical(count_peaks(c(0, 1, 0, 1)), 1L)
  expect_identical(count_peaks(c(1, 0, 1)), 0L)
  expect_identical(count_peaks(c(0, 1, 1, 2.5, 5.1, 4.9)), 1L)
  expect_identical(count_peaks(c(0, 1, 1, 2.5, 5.1, 4.9), min_change = 0.5), 0L)
  expect_identical(count_peaks(c(0, 1, 1, 0.8, 5.1, 4)), 2L)
  expect_identical(count_peaks(c(0, 1, 1, 0.8, 5.1, 4), min_change = 0.1), 2L)
  expect_identical(count_peaks(c(0, 1, 1, 0.8, 5.1, 4), min_change = 0.5), 1L)
})



test_that("Scale to reference", {
  tol <- sqrt(.Machine[["double.eps"]])

  x <- c(1:6, 8)
  x0 <- c(10, -1)
  x0NA <- c(x0, NA)

  # Scale to retain value of reference maximum
  x_scaled <- scale_to_reference_fun(x, x0, fun = max)
  expect_identical(max(x0), max(x_scaled))

  # Scale to retain value of reference sum
  x_scaled <- scale_to_reference_fun(x, x0, fun = sum)
  expect_identical(sum(x0), sum(x_scaled))

  # Scale to retain value of reference mean
  x_scaled <- scale_to_reference_fun(x, x0, fun = mean)
  expect_equal(mean(x0), mean(x_scaled), tolerance = tol)

  # Scale to retain value of reference length (Note: somewhat nonsensical!)
  x_scaled <- scale_to_reference_fun(x, x0, fun = length, na.rm = TRUE)
  expect_identical(x * length(x0) / length(x), x_scaled)

  # Scale and handle NAs
  x_scaled <- scale_to_reference_fun(x, x0NA, fun = mean, na.rm = FALSE)
  expect_true(all(is.na(x_scaled)))

  x_scaled <- scale_to_reference_fun(x, x0NA, fun = max, na.rm = TRUE)
  expect_identical(max(x0, na.rm = TRUE), max(x_scaled))


  # Scale to reference peak frequency
  x <- c(
    0.685, 0.698, 0.717, 1.026, 1.216, 1.239, 1.123, 1.104, 0.999,
    0.81, 0.652, 0.633
  )
  x0 <- c(0.5, 0.5, 0.5, 0.7, 0.9, 1, 1, 1, 0.9, 0.7, 0.5, 0.5)

  x_scaled1 <- scale_to_reference_peak_frequency(x, x0, cap_at_peak = TRUE)
  expect_identical(sum(x0 < max(x0)), sum(x_scaled1 < max(x0)))

  x_scaled2 <- scale_to_reference_peak_frequency(x, x0, cap_at_peak = FALSE)
  expect_identical(sum(x0 < max(x0)), sum(x_scaled2 < max(x0)))
  expect_identical(
    x_scaled1,
    squash_into_low_high(x_scaled2, val_low = -Inf, val_high = max(x0))
  )

  # Scale to sum to 1
  x_scaled1 <- scale_to_reference_fun(x, 1, fun = sum)
  expect_identical(1, sum(x_scaled1))

  x_scaled2 <- scale_by_sum(x)
  expect_identical(1, sum(x_scaled2))
  expect_identical(x_scaled1, x_scaled2)


  # Scale rounded values to sum to 1
  expect_warning(
    scale_rounded_by_sum(x, digits = 1, icolumn_adjust = 7)
  )

  x_scaled1 <- scale_rounded_by_sum(x, digits = 2, icolumn_adjust = 7)
  expect_identical(1, sum(x_scaled1))

  x_scaled1 <- scale_rounded_by_sum(x, digits = 4, icolumn_adjust = 7)
  expect_identical(1, sum(x_scaled1))

  xm <- rbind(x, x + 1, x^2)

  expect_error(
    scale_rounded_by_sum(xm, digits = 1, icolumn_adjust = 7)
  )

  x_scaled1 <- scale_rounded_by_sum(xm, digits = 4, icolumn_adjust = 7)
  expect_identical(rep(1, nrow(xm)), rowSums(x_scaled1))

  xdf <- as.data.frame(xm)
  x_scaled1 <- scale_rounded_by_sum(xdf, digits = 4, icolumn_adjust = 7)
  expect_identical(rep(1, nrow(xm)), rowSums(x_scaled1))
})
