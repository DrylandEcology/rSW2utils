context("Miscellaneous functions")

# Test data
N <- 50
diff <- 0.1
tiny_diff <- 1e-14 # < tolerance = sqrt(.Machine[["double.eps"]])

x <- runif(n = N)

# Set up y1 as identical to x except for one element
y1 <- x
y1[[2L]] <- y1[[2L]] + diff

# Set up y2 as almost identical to y1
y2 <- y1
y2[5:10] <- y2[5:10] + tiny_diff

# Data with lots of NAs
y3 <- rep(NA, N)
y3[[1L]] <- x[[1L]]

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

  expect_identical(
    all_equal_numeric2(y1, x),
    all_equal_numeric2(y2, x)
  )

  expect_match(
    all_equal_numeric2(list(y2, y1), list(x, x))[[1L]],
    "Mean absolute difference"
  )

  expect_match(
    all_equal_numeric2(list(y2, y1), list(x, x, x))[[1L]],
    "Lengths"
  )

  expect_match(
    all_equal_numeric2(list(a = y2, y1), list(x, x))[[1L]],
    "names for target but not for current"
  )

  # Partially non-numeric objects
  expect_true(all_equal_numeric2(iris2, iris2))

  expect_match(
    all_equal_numeric2(iris, iris2, scaled = FALSE)[[1L]],
    "Mean absolute difference"
  )

  # Non-numeric objects
  expect_true(all_equal_numeric2(iris2[, "Species"], iris2[, "Species"]))

  expect_match(
    all_equal_numeric2(iris[, "Species"], iris2[, "Species"]),
    "string mismatch"
  )

  # Handling NAs
  expect_true(all_equal_numeric2(y3, y3, scaled = FALSE))
  # - all non-NA values are equal
  expect_true(all_equal_numeric2(y3, x, scaled = FALSE))
  # - current has more NAs than target
  expect_match(
    all_equal_numeric2(x, y3, scaled = FALSE),
    "'is.NA' value mismatch"
  )
})


test_that("Chunking", {
  tol <- sqrt(.Machine[["double.eps"]])

  vals_nx <- as.integer(c(0, 1, 4, 15, 16, 1000))
  n_chunks <- as.integer(c(0, 1, 3))
  chunk_size <- as.integer(c(0, 1, 5))

  for (nx in vals_nx) {
    for (k in seq_along(n_chunks)) {
      tmp1 <- make_chunks(nx = nx, n_chunks = n_chunks[k])
      expect_identical(
        length(unlist(tmp1)),
        if (n_chunks[k] == 0L) 0L else nx
      )
      expect_identical(length(tmp1), min(nx, n_chunks[k]))

      tmp2 <- make_chunks(nx, chunk_size = chunk_size[k])
      expect_identical(
        length(unlist(tmp2)),
        if (chunk_size[k] == 0L) 0L else nx
      )
      expect_true(all(lengths(tmp2) <= chunk_size[k]))
    }
  }
})
