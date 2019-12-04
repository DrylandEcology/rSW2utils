context("Code style and good package practices")



#--- Code style
test_that("Package code style", {
  # Check locally and on travis
  skip_on_cran()
  skip_on_appveyor()
  skip_if_not_installed("lintr", minimum_version = "2.0.0")


  lintr::expect_lint_free()
})
