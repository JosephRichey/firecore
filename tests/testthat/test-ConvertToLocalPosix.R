library(lubridate)

# Test UTC datetimes
test_dt_utc <- as.POSIXct("2025-01-15 21:30:45", tz = "UTC")
test_date <- as.Date("2025-01-15")

test_that("ConvertToLocalPosix converts UTC datetime to local datetime", {
  result <- ConvertToLocalPosix(test_dt_utc, input = "datetime", output = "datetime")

  expect_s3_class(result, "POSIXct")
  # UTC 21:30 -> Denver 14:30 (MST is UTC-7 in January)
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 14:30:45")
  expect_equal(attr(result, "tzone"), "America/Denver")
})

test_that("ConvertToLocalPosix converts UTC datetime to local date", {
  result <- ConvertToLocalPosix(test_dt_utc, input = "datetime", output = "date")

  expect_s3_class(result, "Date")
  expect_equal(as.character(result), "2025-01-15")
})

test_that("ConvertToLocalPosix handles date to date conversion", {
  result <- ConvertToLocalPosix(test_date, input = "date", output = "date")

  expect_s3_class(result, "Date")
  expect_equal(result, test_date)
})

test_that("ConvertToLocalPosix uses default arguments correctly", {
  # Defaults: input = "datetime", output = "datetime"
  result <- ConvertToLocalPosix(test_dt_utc)

  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 14:30:45")
})

test_that("ConvertToLocalPosix rejects invalid input-output combinations", {
  # date input cannot be converted to datetime output
  expect_error(
    ConvertToLocalPosix(test_date, input = "date", output = "datetime"),
    "Invalid conversion: cannot format input type 'date' as output type 'datetime'"
  )
})

test_that("ConvertToLocalPosix handles date boundary crossings", {
  # Late night UTC that crosses into next day in Denver
  dt_late_utc <- as.POSIXct("2025-01-16 05:30:00", tz = "UTC")
  result <- ConvertToLocalPosix(dt_late_utc, output = "datetime")

  # UTC 05:30 -> Denver 22:30 previous day (MST is UTC-7)
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 22:30:00")

  # When converted to date, should be the previous day
  result_date <- ConvertToLocalPosix(dt_late_utc, output = "date")
  expect_equal(as.character(result_date), "2025-01-15")
})

test_that("ConvertToLocalPosix handles early morning UTC crossing to previous day", {
  # Early morning UTC that's still previous day in Denver
  dt_early_utc <- as.POSIXct("2025-01-16 02:00:00", tz = "UTC")
  result <- ConvertToLocalPosix(dt_early_utc, output = "datetime")

  # UTC 02:00 -> Denver 19:00 previous day
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 19:00:00")

  result_date <- ConvertToLocalPosix(dt_early_utc, output = "date")
  expect_equal(as.character(result_date), "2025-01-15")
})

test_that("ConvertToLocalPosix handles midnight conversions correctly", {
  # Midnight UTC
  dt_midnight_utc <- as.POSIXct("2025-01-16 00:00:00", tz = "UTC")
  result <- ConvertToLocalPosix(dt_midnight_utc, output = "datetime")

  # UTC 00:00 Jan 16 -> Denver 17:00 Jan 15
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 17:00:00")
})

test_that("ConvertToLocalPosix handles year boundary crossing", {
  # New Year's Eve in UTC, still previous year in Denver
  dt_nye_utc <- as.POSIXct("2025-01-01 05:00:00", tz = "UTC")
  result <- ConvertToLocalPosix(dt_nye_utc, output = "datetime")

  # UTC 05:00 Jan 1 -> Denver 22:00 Dec 31
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2024-12-31 22:00:00")

  result_date <- ConvertToLocalPosix(dt_nye_utc, output = "date")
  expect_equal(as.character(result_date), "2024-12-31")
})

test_that("ConvertToLocalPosix handles leap day correctly", {
  # Leap day in UTC
  dt_leap_utc <- as.POSIXct("2024-02-29 12:00:00", tz = "UTC")
  result <- ConvertToLocalPosix(dt_leap_utc, output = "datetime")

  # UTC 12:00 -> Denver 05:00 (MST is UTC-7)
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2024-02-29 05:00:00")

  result_date <- ConvertToLocalPosix(dt_leap_utc, output = "date")
  expect_equal(as.character(result_date), "2024-02-29")
})

test_that("ConvertToLocalPosix handles DST transition - spring forward", {
  # March 9, 2025 - DST spring forward day in Denver
  # UTC time that falls during the transition
  dt_dst_utc <- as.POSIXct("2025-03-09 09:00:00", tz = "UTC")
  result <- ConvertToLocalPosix(dt_dst_utc, output = "datetime")

  # UTC 09:00 -> Denver 03:00 (after spring forward, now MDT UTC-6)
  # The 2:00-2:59 AM hour doesn't exist
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-03-09 03:00:00")
})

test_that("ConvertToLocalPosix handles DST transition - fall back", {
  # November 2, 2025 - DST fall back day in Denver
  # Two different UTC times that map to same local time

  # First 1:30 AM (still MDT, UTC-6)
  dt_fall_1_utc <- as.POSIXct("2025-11-02 07:30:00", tz = "UTC")
  result_1 <- ConvertToLocalPosix(dt_fall_1_utc, output = "datetime")
  expect_equal(format(result_1, "%Y-%m-%d %H:%M:%S"), "2025-11-02 01:30:00")

  # Second 1:30 AM (now MST, UTC-7)
  dt_fall_2_utc <- as.POSIXct("2025-11-02 08:30:00", tz = "UTC")
  result_2 <- ConvertToLocalPosix(dt_fall_2_utc, output = "datetime")
  expect_equal(format(result_2, "%Y-%m-%d %H:%M:%S"), "2025-11-02 01:30:00")

  # Both show same local time but represent different moments
  expect_true(dt_fall_1_utc < dt_fall_2_utc)
})

test_that("ConvertToLocalPosix handles summer vs winter timezone offset", {
  # January (MST, UTC-7)
  dt_winter_utc <- as.POSIXct("2025-01-15 20:00:00", tz = "UTC")
  result_winter <- ConvertToLocalPosix(dt_winter_utc, output = "datetime")
  expect_equal(format(result_winter, "%Y-%m-%d %H:%M:%S"), "2025-01-15 13:00:00")

  # July (MDT, UTC-6)
  dt_summer_utc <- as.POSIXct("2025-07-15 20:00:00", tz = "UTC")
  result_summer <- ConvertToLocalPosix(dt_summer_utc, output = "datetime")
  expect_equal(format(result_summer, "%Y-%m-%d %H:%M:%S"), "2025-07-15 14:00:00")

  # Same UTC time, different local times due to DST
  expect_equal(hour(result_winter), 13)
  expect_equal(hour(result_summer), 14)
})

test_that("ConvertToLocalPosix works with vector inputs", {
  dt_vector_utc <- c(
    as.POSIXct("2025-01-15 21:30:00", tz = "UTC"),
    as.POSIXct("2025-02-20 15:00:00", tz = "UTC")
  )

  result <- ConvertToLocalPosix(dt_vector_utc, output = "datetime")

  expect_length(result, 2)
  expect_equal(format(result[1], "%Y-%m-%d %H:%M:%S"), "2025-01-15 14:30:00")
  expect_equal(format(result[2], "%Y-%m-%d %H:%M:%S"), "2025-02-20 08:00:00")
})

test_that("ConvertToLocalPosix preserves sub-second precision", {
  dt_precise_utc <- as.POSIXct("2025-01-15 21:30:45.123456", tz = "UTC")
  result <- ConvertToLocalPosix(dt_precise_utc, output = "datetime")

  # Check that fractional seconds are preserved
  expect_equal(
    format(result, "%Y-%m-%d %H:%M:%OS6"),
    "2025-01-15 14:30:45.123456"
  )
})

test_that("ConvertToLocalPosix handles character datetime input", {
  # Should work with character strings that can be coerced to POSIXct
  dt_char <- "2025-01-15 21:30:00"
  result <- ConvertToLocalPosix(dt_char, input = "datetime", output = "datetime")

  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 14:30:00")
})
