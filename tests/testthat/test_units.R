
#--- TESTS
# Test 'convert_temperature'
temp_C <- c(-50, 0, 50)
temp_F <- c(-58, 32, 122)
temp_K <- 273.15 + temp_C
# Test 'convert_precipitation'
dpm <- c(31, 28, 31)
tmp_ppt <- c(1.5, 0.3, 0)

test_that("Unit conversion", {
  expect_identical(convert_temperature(temp_C, unit_from = "C"), temp_C)
  expect_equal(
    convert_temperature(temp_F, unit_from = "F"),
    temp_C,
    tolerance = 1e-6
  )
  expect_identical(convert_temperature(temp_K, unit_from = "K"), temp_C)
  expect_error(convert_temperature(temp_F, unit_from = "degree F"))
  expect_error(convert_temperature(temp_K, unit_from = "K", unit_to = "F"))

  # unit_to is "cm/month"
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = NA,
      unit_from = "cm/month", # nolint: nonportable_path_linter.
      unit_to = "cm/month" # nolint: nonportable_path_linter.
    ),
    tmp_ppt
  )
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = NA,
      unit_from = "mm/month", # nolint: nonportable_path_linter.
      unit_to = "cm/month" # nolint: nonportable_path_linter.
    ),
    tmp_ppt / 10
  )
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = dpm,
      unit_from = "mm/d", # nolint: nonportable_path_linter.
      unit_to = "cm/month" # nolint: nonportable_path_linter.
    ),
    tmp_ppt * dpm / 10
  )
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = dpm,
      unit_from = "cm/d", # nolint: nonportable_path_linter.
      unit_to = "cm/month" # nolint: nonportable_path_linter.
    ),
    tmp_ppt * dpm
  )
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = dpm,
      unit_from = "kg m-2 s-1",
      unit_to = "cm/month" # nolint: nonportable_path_linter.
    ),
    tmp_ppt * dpm * 8640
  )

  # unit_to is "cm/day"
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = dpm,
      unit_from = "cm/month",  # nolint: nonportable_path_linter.
      unit_to = "cm/day" # nolint: nonportable_path_linter.
    ),
    tmp_ppt / dpm
  )
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = dpm,
      unit_from = "mm/month", # nolint: nonportable_path_linter.
      unit_to = "cm/day" # nolint: nonportable_path_linter.
    ),
    tmp_ppt / (dpm * 10)
  )
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = NA,
      unit_from = "mm/d", # nolint: nonportable_path_linter.
      unit_to = "cm/day" # nolint: nonportable_path_linter.
    ),
    tmp_ppt / 10
  )
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = NA,
      unit_from = "cm/d", # nolint: nonportable_path_linter.
      unit_to = "cm/day" # nolint: nonportable_path_linter.
    ),
    tmp_ppt
  )
  expect_identical(
    convert_precipitation(
      tmp_ppt,
      dpm = NA,
      unit_from = "kg m-2 s-1",
      unit_to = "cm/day" # nolint: nonportable_path_linter.
    ),
    tmp_ppt * 8640
  )

  # errors
  expect_error(convert_precipitation(
    tmp_ppt,
    dpm = dpm,
    unit_from = "L m-2",
    unit_to = "cm/month" # nolint: nonportable_path_linter.
  ))
  expect_error(convert_precipitation(
    tmp_ppt,
    dpm = dpm,
    unit_from = "cm/month", # nolint: nonportable_path_linter.
    unit_to = "L m-2"
  ))
})
