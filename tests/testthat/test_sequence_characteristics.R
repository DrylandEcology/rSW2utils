context("Sequence characteristics")

#--- TESTS
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

      expect_equal(
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
          mf[isnotna]
        )

        # Check na.rm
        mf2 <- moving_function(x = tmp, k = k, win_fun = f, na.rm = TRUE)
        if (!(fun == "sd" && k == 1)) {
          expect_equal(sum(is.na(mf2)), 0)
        }
        expect_equal(mf[isnotna], mf2[isnotna])

        # Check circular
        mf3 <- moving_function(x = tmp, k = k, win_fun = f, na.rm = FALSE,
          circular = TRUE
        )
        if (!(fun == "sd" && k == 1)) {
          expect_equal(sum(is.na(mf3)), 0)
        }
        expect_equal(mf[isnotna], mf3[isnotna])
      }
    }
  }
})


test_that("Counting peaks", {
  expect_equal(count_peaks(c(0, 1, 0, 1, 0)), 2)
  expect_equal(count_peaks(c(0, 1, 0, 1)), 1)
  expect_equal(count_peaks(c(1, 0, 1)), 0)
  expect_equal(count_peaks(c(0, 1, 1, 2.5, 5.1, 4.9)), 1)
  expect_equal(count_peaks(c(0, 1, 1, 2.5, 5.1, 4.9), min_change = 0.5), 0)
  expect_equal(count_peaks(c(0, 1, 1, 0.8, 5.1, 4)), 2)
  expect_equal(count_peaks(c(0, 1, 1, 0.8, 5.1, 4), min_change = 0.1), 2)
  expect_equal(count_peaks(c(0, 1, 1, 0.8, 5.1, 4), min_change = 0.5), 1)
})
