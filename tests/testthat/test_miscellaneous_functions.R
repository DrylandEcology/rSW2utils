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


# Unit tests
test_that("Comparing objects", {
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
})
