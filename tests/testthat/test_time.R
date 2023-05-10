context("Time")

#--- TESTS
test_that("Time information", {
  # Leap years
  expect_true(isLeapYear(1980))
  expect_true(isLeapYear(2000))
  expect_false(isLeapYear(2003))
  expect_true(isLeapYear(2016))
  expect_false(isLeapYear(2100))

  # Sequence of days in a period of years
  expect_length(days_in_years(1980, 1980), 366)
  expect_length(days_in_years(2000, 2000), 366)
  expect_length(days_in_years(2003, 2003), 365)
  expect_length(days_in_years(2016, 2016), 366)
  expect_length(days_in_years(2100, 2100), 365)

  # Sequence of month numbers for each day in the period
  expect_equal(
    seq_month_ofeach_day(list(1980, 1, 1), list(2010, 12, 31), tz = "UTC"),
    # nolint start: extraction_operator_linter.
    as.POSIXlt(days_in_years(1980, 2010))$mon + 1
    # nolint end: extraction_operator_linter.
  )
})
