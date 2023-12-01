# rSW2utils v0.2.1
* New `calc_runs()` returns a list with integer sequences of the position(s) of
  runs (sequences) of `TRUE` values (@dschlaep).
* Improved `impute_df()` (@dschlaep):
  * Works now with matrices and data frames.
  * Code refactored to identify "runs" (sets of consecutive missing values);
    imputation methods are now applied per "run"
    (instead of to one missing value at a time).
  * Gained imputation type `"interp"`; this method replaces missing values
    by linear interpolation (or extrapolation) using the two closest neighbors
    (and assuming that rows represent equidistant steps).
  * Gained argument `"nmax_run"`; runs of missing values longer
    than `"nmax_run"` are not imputed.


# rSW2utils v0.2.0
* This version only includes package maintenance:
* Linting updated to `lintr` >= 3 and
  lint workflow switched from package tests to Github Action (#3; @dschlaep).
* Github Actions are triggered for `release/**` branches in addition to `main`.
* `r-lib` Github Actions updated to `v2` (#5; @dschlaep).
* Unit tests updated to `testthat` edition 3 (#4; @dschlaep).


# rSW2utils v0.1.0
* Initial release

