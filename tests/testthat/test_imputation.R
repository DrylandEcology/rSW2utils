
#---TESTS
test_that("Impute missing values", {
  #--- Prepare test data
  Nrows <- 30
  Nmiss <- 5

  df_template <- data.frame(
    linear = seq_len(Nrows),
    all_missing = NA,
    all_same = 1,
    cyclic = cos(2 * pi * seq_len(Nrows) / Nrows)
  )

  vars <- colnames(df_template)
  vars_with_values <- vars[!apply(df_template, 2, anyNA)]

  types <- c("mean", "locf", "interp")
  cyclicity <- c(FALSE, TRUE)


  #------- 'None' imputation returns inputs unchanged
  expect_identical(
    impute_df(df_template, imputation_type = "none"),
    df_template
  )


  #-------
  # Check 1: If every value is missing, then no numeric values are imputed.
  # Check 2: If missing values remain, then a warning is issued.
  # Check 3: If no value is missing, then no value is modified.
  df1 <- df_template

  for (type in types) {
    expect_warning(
      res1 <- impute_df( # nolint: implicit_assignment_linter.
        x = df1,
        imputation_type = type
      )
    )
    expect_identical(df1, res1)
  }


  #------- Last values are missing
  df2 <- df_template[, vars_with_values]
  idsNA <- (Nrows - Nmiss):Nrows
  df2[idsNA, ] <- NA

  for (type in types) {
    res2 <- expect_silent(
      impute_df(
        x = df2,
        imputation_type = type,
        cyclic = FALSE
      )
    )

    for (v in vars_with_values) {
      expect_false(anyNA(res2[, v]))

      if (type == "locf") {
        expect_true(all(res2[idsNA, v] == df2[idsNA[[1L]] - 1, v]))
      }
    }
  }


  #------- First values are missing
  df3 <- df_template[, vars_with_values]
  idsNA <- 1:Nmiss
  df3[idsNA, ] <- NA

  for (type in types) {
    for (cyclic in cyclicity) {

      if (type == "locf" && !cyclic) {
        expect_warning(
          res3 <- impute_df( # nolint: implicit_assignment_linter.
            x = df3,
            imputation_type = type,
            cyclic = cyclic
          )
        )

      } else {
        expect_silent(
          res3 <- impute_df( # nolint: implicit_assignment_linter.
            x = df3,
            imputation_type = type,
            cyclic = cyclic
          )
        )
      }

      for (v in vars_with_values) {
        if (type == "locf") {
          if (cyclic) {
            # last value is imputed
            expect_true(all(res3[idsNA, v] == df3[Nrows, v]))
          } else {
            # no value to can be imputed --> NAs
            expect_true(all(is.na(res3[idsNA, v])))
          }
        } else if (type == "mean") {
          expect_false(anyNA(res3[, v]))
        }
      }
    }
  }
})
