
#--- Tests
test_that("Monotonicity:", {
  #--- INPUTS
  margins <- c("byrow", "bycolumn")

  tmp <- matrix(NA, nrow = 5, ncol = 3)
  # nolint start
  test_matrices <- list(
    x1 = tmp,
    x2 = {
      x <- tmp
      x[] <- 0
      x
    },
    x3 = x3 <- {
      x <- tmp
      x[] <- 1:15
      x
    },
    x4 = {
      x <- x3
      x[2, 3] <- x[2, 2]
      x
    },
    x5 = {
      x <- x3
      x[2, 2:3] <- NA
      x
    },
    x6 = {
      x <- x3
      x[2, 3] <- 0
      x
    }
  )
  # nolint end

  replacement <- -99

  # Expected outputs
  good_nonstrict_matrices <- test_matrices
  good_nonstrict_matrices[["x6"]] <- {
    x <- test_matrices[["x6"]]
    x[2, 3] <- replacement
    x
  }

  good_strict_matrices <- test_matrices
  good_strict_matrices[["x1"]] <- {
    x <- test_matrices[["x1"]]
    x[] <- replacement # nolint: extraction_operator_linter.
    x
  }
  good_strict_matrices[["x2"]] <- {
    x <- test_matrices[["x2"]]
    x[, -1] <- replacement
    x
  }
  good_strict_matrices[["x4"]] <- {
    x <- test_matrices[["x4"]]
    x[2, 3] <- replacement
    x
  }
  good_strict_matrices[["x5"]] <- {
    x <- test_matrices[["x5"]]
    x[2, 2:3] <- replacement
    x
  }
  good_strict_matrices[["x6"]] <- {
    x <- test_matrices[["x6"]]
    x[2, 3] <- replacement
    x
  }


  #--- TESTS
  for (it in seq_along(margins)) {
    for (k in seq_along(test_matrices)) {
      x <- test_matrices[[k]]
      res_ns <- good_nonstrict_matrices[[k]]
      res_s <- good_strict_matrices[[k]]

      if (it == 2) {
        x <- t(x)
        res_ns <- t(res_ns)
        res_s <- t(res_s)
      }

      expect_identical(
        check_monotonic_increase(
          x,
          MARGIN = it,
          increase = TRUE,
          strictly = FALSE,
          fail = FALSE,
          replacement = replacement,
          na.rm = FALSE
        ),
        res_ns
      )

      expect_identical(
        check_monotonic_increase(
          x,
          MARGIN = it,
          increase = TRUE,
          strictly = TRUE,
          fail = FALSE,
          replacement = replacement,
          na.rm = FALSE
        ),
        res_s
      )
    }
  }
})
