context("Miscellaneous functions")

# Test data
N <- 50
diff <- 0.1
tiny_diff <- 1e-14 # < tolerance = sqrt(.Machine$double.eps)

x <- runif(n = N)

# Set up y1 as identical to x except for one element
y1 <- x
y1[2] <- y1[2] + diff

# Set up y2 as almost identical to y1
y2 <- y1
y2[5:10] <- y2[5:10] + tiny_diff


utils::data("iris", package = "datasets")
iris[, "Species"] <- as.character(iris[, "Species"])
iris2 <- iris # Column "Species" is a character vector
iris2[1, "Sepal.Length"] <- 0.1 + iris2[1, "Sepal.Length"]
iris2[2:7, "Sepal.Length"] <- iris2[2:7, "Sepal.Length"] + tiny_diff
iris2[10, "Species"] <- "Test"


# Unit tests
test_that("Comparing objects", {
  # Numeric objects
  expect_true(all_equal_numeric2(x, x, scaled = FALSE))
  expect_true(all_equal_numeric2(x, x, scaled = TRUE))

  expect_false(
    all.equal(y1, x, scale = 1, countEQ = FALSE) ==
      all.equal(y2, x, scale = 1, countEQ = FALSE)
  )

  expect_equal(
    all_equal_numeric2(y1, x),
    all_equal_numeric2(y2, x)
  )

  expect_match(
    all_equal_numeric2(list(y2, y1), list(x, x))[[1]],
    "Mean absolute difference"
  )

  expect_match(
    all_equal_numeric2(list(y2, y1), list(x, x, x))[[1]],
    "Lengths"
  )

  expect_match(
    all_equal_numeric2(list(a = y2, y1), list(x, x))[[1]],
    "names for target but not for current"
  )

  # Partially non-numeric objects
  expect_true(all_equal_numeric2(iris2, iris2))

  expect_match(
    all_equal_numeric2(iris, iris2, scaled = FALSE)[[1]],
    "Mean absolute difference"
  )

  # Non-numeric objects
  expect_true(all_equal_numeric2(iris2[, "Species"], iris2[, "Species"]))

  expect_match(
    all_equal_numeric2(iris[, "Species"], iris2[, "Species"]),
    "string mismatch"
  )

})
