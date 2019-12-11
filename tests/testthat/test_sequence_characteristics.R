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
