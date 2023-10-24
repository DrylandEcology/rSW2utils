# rSW2utils v0.2.1-9000
* New `calc_runs()` returns a list with integer sequences of the position(s) of
  runs (sequences) of `TRUE` values.
* `impute_df()` now works on matrices and data frames. It also gains
  imputation type `"interp"` which replaces missing values
  (for each run of missing values separately) by linear interpolation
  (or extrapolation if at the start or end of a sequence) using the
  two closest neighbors assuming that rows represent equidistant steps.

# rSW2utils v0.2.0
* This version only includes package maintenance:
* Linting updated to `lintr` >= 3 and
  lint workflow switched from package tests to Github Action (#3; @dschlaep).
* Github Actions are triggered for `release/**` branches in addition to `main`.
* `r-lib` Github Actions updated to `v2` (#5; @dschlaep).
* Unit tests updated to `testthat` edition 3 (#4; @dschlaep).

# rSW2utils v0.1.0
* Initial release

