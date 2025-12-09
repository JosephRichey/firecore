library(lubridate)
library(hms)

test_that("BuildDateTime builds datetime from character time and date in local timezone to UTC", {
  result <- BuildDateTime(
    time = "14:30:00",
    date = "2025-01-15",
    input = "local",
    return_type = "UTC"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")
  # Local 14:30 MST (UTC-7) -> UTC 21:30
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 21:30:00")
})

test_that("BuildDateTime builds datetime from character time without seconds", {
  result <- BuildDateTime(
    time = "14:30",
    date = "2025-01-15",
    input = "local",
    return_type = "UTC"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 21:30:00")
})

test_that("BuildDateTime extracts time from POSIXct input (Shiny time input)", {
  # Shiny time inputs return POSIXct with arbitrary date (usually 1970-01-01)
  shiny_time <- as.POSIXct("1970-01-01 14:30:00", tz = "UTC")

  result <- BuildDateTime(
    time = shiny_time,
    date = as.Date("2025-01-15"),
    input = "local",
    return_type = "UTC"
  )

  expect_s3_class(result, "POSIXct")
  # Should use the correct date (2025-01-15) not the Shiny date (1970-01-01)
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 21:30:00")
})

test_that("BuildDateTime extracts time from POSIXlt input", {
  posixlt_time <- as.POSIXlt("2020-05-10 09:15:00", tz = "UTC")

  result <- BuildDateTime(
    time = posixlt_time,
    date = as.Date("2025-01-15"),
    input = "local",
    return_type = "UTC"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 16:15:00")
})

test_that("BuildDateTime uses default arguments correctly", {
  # Defaults: input = "local", return_type = "UTC"
  result <- BuildDateTime(time = "14:30:00", date = "2025-01-15")

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 21:30:00")
})

test_that("BuildDateTime returns local timezone when requested", {
  result <- BuildDateTime(
    time = "14:30:00",
    date = "2025-01-15",
    input = "local",
    return_type = "local"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "America/Denver")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 14:30:00")
})

test_that("BuildDateTime handles UTC input to UTC output", {
  result <- BuildDateTime(
    time = "21:30:00",
    date = "2025-01-15",
    input = "UTC",
    return_type = "UTC"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 21:30:00")
})

test_that("BuildDateTime handles UTC input to local output", {
  result <- BuildDateTime(
    time = "21:30:00",
    date = "2025-01-15",
    input = "UTC",
    return_type = "local"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "America/Denver")
  # UTC 21:30 -> Local 14:30 (MST is UTC-7)
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 14:30:00")
})

test_that("BuildDateTime handles Date object input", {
  date_obj <- as.Date("2025-01-15")

  result <- BuildDateTime(
    time = "14:30:00",
    date = date_obj,
    input = "local",
    return_type = "UTC"
  )

  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 21:30:00")
})

test_that("BuildDateTime handles midnight correctly", {
  result <- BuildDateTime(
    time = "00:00:00",
    date = "2025-01-15",
    input = "local",
    return_type = "UTC"
  )

  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 07:00:00")
})

test_that("BuildDateTime handles end of day correctly", {
  result <- BuildDateTime(
    time = "23:59:59",
    date = "2025-01-15",
    input = "local",
    return_type = "UTC"
  )

  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-16 06:59:59")
})

test_that("BuildDateTime handles noon correctly", {
  result <- BuildDateTime(
    time = "12:00:00",
    date = "2025-01-15",
    input = "local",
    return_type = "UTC"
  )

  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-15 19:00:00")
})

test_that("BuildDateTime handles leap day", {
  result <- BuildDateTime(
    time = "14:30:00",
    date = "2024-02-29",
    input = "local",
    return_type = "UTC"
  )

  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2024-02-29 21:30:00")
})

test_that("BuildDateTime handles year boundary", {
  # New Year's Eve
  result <- BuildDateTime(
    time = "23:30:00",
    date = "2024-12-31",
    input = "local",
    return_type = "UTC"
  )

  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-01-01 06:30:00")

  # New Year's Day
  result2 <- BuildDateTime(
    time = "00:30:00",
    date = "2025-01-01",
    input = "local",
    return_type = "UTC"
  )

  expect_equal(format(result2, "%Y-%m-%d %H:%M:%S"), "2025-01-01 07:30:00")
})

test_that("BuildDateTime errors on non existent hour,  DST - spring forward", {
  # March 9, 2025 at 2:30 AM doesn't exist (nonexistent hour)
  expect_error(BuildDateTime(
    time = "02:30:00",
    date = "2025-03-09",
    input = "local",
    return_type = "UTC"
  ))
})

test_that("BuildDateTime handles DST - hour before spring forward", {
  # March 9, 2025 at 1:30 AM MST (before spring forward)
  result <- BuildDateTime(
    time = "01:30:00",
    date = "2025-03-09",
    input = "local",
    return_type = "UTC"
  )

  # Local 1:30 MST (UTC-7) -> UTC 08:30
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-03-09 08:30:00")
})

test_that("BuildDateTime handles DST - hour after spring forward", {
  # March 9, 2025 at 3:30 AM MDT (after spring forward)
  result <- BuildDateTime(
    time = "03:30:00",
    date = "2025-03-09",
    input = "local",
    return_type = "UTC"
  )

  # Local 3:30 MDT (UTC-6) -> UTC 09:30
  expect_equal(format(result, "%Y-%m-%d %H:%M:%S"), "2025-03-09 09:30:00")
})

test_that("BuildDateTime errors on ambiguous hour", {
  # November 2, 2025 at 1:30 AM (ambiguous - occurs twice)
  # Throw error
  expect_error(BuildDateTime(
    time = "01:30:00",
    date = "2025-11-02",
    input = "local",
    return_type = "UTC"
  ))
})

test_that("BuildDateTime handles summer vs winter timezone offset", {
  # January (MST, UTC-7)
  result_winter <- BuildDateTime(
    time = "14:00:00",
    date = "2025-01-15",
    input = "local",
    return_type = "UTC"
  )
  expect_equal(
    format(result_winter, "%Y-%m-%d %H:%M:%S"),
    "2025-01-15 21:00:00"
  )

  # July (MDT, UTC-6)
  result_summer <- BuildDateTime(
    time = "14:00:00",
    date = "2025-07-15",
    input = "local",
    return_type = "UTC"
  )
  expect_equal(
    format(result_summer, "%Y-%m-%d %H:%M:%S"),
    "2025-07-15 20:00:00"
  )

  # Same local time, different UTC times due to DST
  expect_equal(hour(result_winter), 21)
  expect_equal(hour(result_summer), 20)
})

test_that("BuildDateTime handles various time formats", {
  # HH:MM:SS
  result1 <- BuildDateTime("14:30:00", "2025-01-15", return_type = "local")
  expect_equal(format(result1, "%H:%M:%S"), "14:30:00")

  # HH:MM
  result2 <- BuildDateTime("14:30", "2025-01-15", return_type = "local")
  expect_equal(format(result2, "%H:%M:%S"), "14:30:00")

  # H:MM (single digit hour)
  result3 <- BuildDateTime("9:30", "2025-01-15", return_type = "local")
  expect_equal(format(result3, "%H:%M:%S"), "09:30:00")
})

test_that("BuildDateTime preserves sub-second precision in time", {
  result <- BuildDateTime(
    time = "14:30:45.123456",
    date = "2025-01-15",
    input = "local",
    return_type = "local"
  )

  expect_equal(
    format(result, "%Y-%m-%d %H:%M:%OS6"),
    "2025-01-15 14:30:45.123456"
  )
})

test_that("BuildDateTime round-trip conversion preserves datetime", {
  # Build, convert to local, should match original
  original_time <- "14:30:00"
  original_date <- "2025-01-15"

  # Build to UTC
  dt_utc <- BuildDateTime(
    original_time,
    original_date,
    input = "local",
    return_type = "UTC"
  )

  # Convert back to local
  dt_local <- with_tz(dt_utc, "America/Denver")

  expect_equal(format(dt_local, "%Y-%m-%d"), original_date)
  expect_equal(format(dt_local, "%H:%M:%S"), original_time)
})
