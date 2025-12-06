library(lubridate)

# Test datetime and date objects
test_dt <- as.POSIXct("2025-01-15 14:30:45", tz = "America/Denver")
test_date <- as.Date("2025-01-15")

test_that("FormatDateTime formats datetime to datetime in local timezone", {
  result <- FormatDateTime(test_dt, input = "datetime", output = "datetime", target_tz = "local")

  expect_type(result, "character")
  # Should match the date_time_format setting: "%m-%d-%Y %H:%M"
  expect_match(result, "^\\d{2}-\\d{2}-\\d{4} \\d{2}:\\d{2}$")
  expect_equal(result, "01-15-2025 14:30")
})

test_that("FormatDateTime formats datetime to date", {
  result <- FormatDateTime(test_dt, input = "datetime", output = "date")

  expect_type(result, "character")
  # Should match the date_format setting: "%m-%d-%Y"
  expect_match(result, "^\\d{2}-\\d{2}-\\d{4}$")
  expect_equal(result, "01-15-2025")
})

test_date_2 <- as.POSIXct("2025-01-15 20:30:45", tz = "America/Denver")
test_that("FormatDateTime formats datetime to date in different time zone", {
  result <- FormatDateTime(test_date_2, input = "datetime", output = "date", target_tz = 'UTC')

  expect_type(result, "character")
  # Should match the date_format setting: "%m-%d-%Y"
  expect_match(result, "^\\d{2}-\\d{2}-\\d{4}$")
  expect_equal(result, "01-16-2025")
})

test_that("FormatDateTime formats datetime to time without seconds", {
  result <- FormatDateTime(test_dt, input = "datetime", output = "time", seconds = FALSE)

  expect_type(result, "character")
  expect_match(result, "^\\d{2}:\\d{2}$")
  expect_equal(result, "14:30")
})

test_that("FormatDateTime formats datetime to time with seconds", {
  result <- FormatDateTime(test_dt, input = "datetime", output = "time", seconds = TRUE)

  expect_type(result, "character")
  expect_match(result, "^\\d{2}:\\d{2}:\\d{2}$")
  expect_equal(result, "14:30:45")
})

test_that("FormatDateTime formats datetime to UTC", {
  result <- FormatDateTime(test_dt, input = "datetime", output = "datetime", target_tz = "UTC")

  expect_type(result, "character")
  # UTC format should always be "%Y-%m-%d %H:%M:%S"
  expect_match(result, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
  # Denver is UTC-7 (MST) or UTC-6 (MDT), so time will be different
  # In January, Denver is MST (UTC-7), so 14:30 Denver = 21:30 UTC
  expect_equal(result, "2025-01-15 21:30:45")
})

test_that("FormatDateTime formats date input to date output", {
  result <- FormatDateTime(test_date, input = "date", output = "date")

  expect_type(result, "character")
  expect_match(result, "^\\d{2}-\\d{2}-\\d{4}$")
  expect_equal(result, "01-15-2025")
})

test_that("FormatDateTime uses default arguments correctly", {
  # Defaults: input = "datetime", output = "datetime", target_tz = "local", seconds = FALSE
  result <- FormatDateTime(test_dt)

  expect_type(result, "character")
  expect_equal(result, "01-15-2025 14:30")
})

test_that("FormatDateTime rejects invalid input-output combinations", {
  # date input cannot be formatted as time
  expect_error(
    FormatDateTime(test_date, input = "date", output = "time"),
    "Invalid conversion: cannot format input type 'date' as output type 'time'"
  )

  # date input cannot be formatted as datetime
  expect_error(
    FormatDateTime(test_date, input = "date", output = "datetime"),
    "Invalid conversion: cannot format input type 'date' as output type 'datetime'"
  )
})

test_that("FormatDateTime handles different timezones correctly", {
  # Create datetime in different timezone
  dt_utc <- as.POSIXct("2025-01-15 21:30:45", tz = "UTC")

  result_local <- FormatDateTime(dt_utc, output = "datetime", target_tz = "local")
  result_utc <- FormatDateTime(dt_utc, output = "datetime", target_tz = "UTC")

  # Both should be valid but show different times
  expect_type(result_local, "character")
  expect_type(result_utc, "character")
  expect_equal(result_utc, "2025-01-15 21:30:45")
})

test_that("FormatDateTime handles edge cases for dates", {
  # Leap day
  leap_date <- as.Date("2024-02-29")
  result <- FormatDateTime(leap_date, input = "date", output = "date")
  expect_equal(result, "02-29-2024")

  # Year boundary
  new_year <- as.POSIXct("2025-01-01 00:00:00", tz = "America/Denver")
  result <- FormatDateTime(new_year, output = "datetime")
  expect_equal(result, "01-01-2025 00:00")

  # End of day
  end_day <- as.POSIXct("2025-12-31 23:59:59", tz = "America/Denver")
  result <- FormatDateTime(end_day, output = "time", seconds = TRUE)
  expect_equal(result, "23:59:59")
})

test_that("FormatDateTime maintains consistency across timezone conversions", {
  # Same moment in time, different representations
  dt_local <- as.POSIXct("2025-06-15 12:00:00", tz = "America/Denver")

  result_local <- FormatDateTime(dt_local, output = "datetime", target_tz = "local")
  result_utc <- FormatDateTime(dt_local, output = "datetime", target_tz = "UTC")

  expect_equal(result_local, "06-15-2025 12:00")
  # In June, Denver is MDT (UTC-6), so 12:00 Denver = 18:00 UTC
  expect_equal(result_utc, "2025-06-15 18:00:00")
})

test_that("FormatDateTime respects seconds parameter only for time output", {
  # seconds parameter should only affect time output
  result_time_no_sec <- FormatDateTime(test_dt, output = "time", seconds = FALSE)
  result_time_with_sec <- FormatDateTime(test_dt, output = "time", seconds = TRUE)

  expect_equal(result_time_no_sec, "14:30")
  expect_equal(result_time_with_sec, "14:30:45")

  # seconds parameter shouldn't affect datetime output (which uses setting format)
  result_datetime <- FormatDateTime(test_dt, output = "datetime", seconds = TRUE)
  expect_equal(result_datetime, "01-15-2025 14:30")
})

test_that("FormatDateTime works with vector inputs", {
  # Test with multiple datetimes
  dt_vector <- c(
    as.POSIXct("2025-01-15 14:30:45", tz = "America/Denver"),
    as.POSIXct("2025-02-20 09:15:30", tz = "America/Denver")
  )

  result <- FormatDateTime(dt_vector, output = "date")

  expect_length(result, 2)
  expect_equal(result[1], "01-15-2025")
  expect_equal(result[2], "02-20-2025")
})

# Daylight Saving Time Tests
# In America/Denver (Mountain Time):
# - Spring forward: 2nd Sunday in March at 2:00 AM -> 3:00 AM (lose 1 hour)
# - Fall back: 1st Sunday in November at 2:00 AM -> 1:00 AM (gain 1 hour)

test_that("FormatDateTime handles spring forward - hour before DST", {
  # March 9, 2025 at 1:30 AM MST (30 minutes before spring forward)
  dt_before <- as.POSIXct("2025-03-09 01:30:00", tz = "America/Denver")

  result_local <- FormatDateTime(dt_before, output = "datetime", target_tz = "local")
  result_utc <- FormatDateTime(dt_before, output = "datetime", target_tz = "UTC")

  expect_equal(result_local, "03-09-2025 01:30")
  # MST is UTC-7
  expect_equal(result_utc, "2025-03-09 08:30:00")
})

test_that("FormatDateTime handles spring forward - hour after DST", {
  # March 9, 2025 at 3:30 AM MDT (30 minutes after spring forward)
  dt_after <- as.POSIXct("2025-03-09 03:30:00", tz = "America/Denver")

  result_local <- FormatDateTime(dt_after, output = "datetime", target_tz = "local")
  result_utc <- FormatDateTime(dt_after, output = "datetime", target_tz = "UTC")

  expect_equal(result_local, "03-09-2025 03:30")
  # MDT is UTC-6
  expect_equal(result_utc, "2025-03-09 09:30:00")
})

test_that("FormatDateTime handles fall back - hour before DST", {
  # November 2, 2025 at 12:30 AM MDT (90 minutes before fall back)
  dt_before <- as.POSIXct("2025-11-02 00:30:00", tz = "America/Denver")

  result_local <- FormatDateTime(dt_before, output = "datetime", target_tz = "local")
  result_utc <- FormatDateTime(dt_before, output = "datetime", target_tz = "UTC")

  expect_equal(result_local, "11-02-2025 00:30")
  # MDT is UTC-6
  expect_equal(result_utc, "2025-11-02 06:30:00")
})

test_that("FormatDateTime handles fall back - ambiguous hour (first occurrence)", {
  # November 2, 2025 at 1:30 AM - this time occurs twice!
  # First as 1:30 AM MDT, then again as 1:30 AM MST
  # We can force this by creating it from UTC (MDT)
  dt_ambiguous <- with_tz(as.POSIXct("2025-11-02 07:30:00", tz = "UTC"),
                          "America/Denver")

  result_local <- FormatDateTime(dt_ambiguous, output = "datetime", target_tz = "local")
  result_utc <- FormatDateTime(dt_ambiguous, output = "datetime", target_tz = "UTC")

  expect_equal(result_local, "11-02-2025 01:30")
  # First occurrence is MDT (UTC-6)
  expect_equal(result_utc, "2025-11-02 07:30:00")
})

test_that("FormatDateTime handles fall back - ambiguous hour (second occurrence)", {
  # November 2, 2025 at 1:30 AM MST (second occurrence after falling back)
  # We can force this by creating it from UTC
  dt_ambiguous_2nd <- with_tz(as.POSIXct("2025-11-02 08:30:00", tz = "UTC"),
                              "America/Denver")

  result_local <- FormatDateTime(dt_ambiguous_2nd, output = "datetime", target_tz = "local")
  result_utc <- FormatDateTime(dt_ambiguous_2nd, output = "datetime", target_tz = "UTC")

  expect_equal(result_local, "11-02-2025 01:30")
  # Second occurrence is MST (UTC-7)
  expect_equal(result_utc, "2025-11-02 08:30:00")
})

test_that("FormatDateTime handles fall back - hour after DST", {
  # November 2, 2025 at 3:30 AM MST (after fall back completes)
  dt_after <- as.POSIXct("2025-11-02 03:30:00", tz = "America/Denver")

  result_local <- FormatDateTime(dt_after, output = "datetime", target_tz = "local")
  result_utc <- FormatDateTime(dt_after, output = "datetime", target_tz = "UTC")

  expect_equal(result_local, "11-02-2025 03:30")
  # MST is UTC-7
  expect_equal(result_utc, "2025-11-02 10:30:00")
})

test_that("FormatDateTime UTC output is immune to DST changes", {
  # Compare same UTC time before and after DST
  utc_time_before_spring <- as.POSIXct("2025-03-09 08:00:00", tz = "UTC")
  utc_time_after_spring <- as.POSIXct("2025-03-09 09:00:00", tz = "UTC")

  result_before <- FormatDateTime(utc_time_before_spring, output = "datetime", target_tz = "UTC")
  result_after <- FormatDateTime(utc_time_after_spring, output = "datetime", target_tz = "UTC")

  # UTC should always be consistent
  expect_equal(result_before, "2025-03-09 08:00:00")
  expect_equal(result_after, "2025-03-09 09:00:00")
})

test_that("FormatDateTime handles date-only formatting across DST boundary", {
  # Dates shouldn't be affected by time changes
  date_before_dst <- as.Date("2025-03-08")
  date_of_dst <- as.Date("2025-03-09")
  date_after_dst <- as.Date("2025-03-10")

  result_before <- FormatDateTime(date_before_dst, input = "date", output = "date")
  result_of <- FormatDateTime(date_of_dst, input = "date", output = "date")
  result_after <- FormatDateTime(date_after_dst, input = "date", output = "date")

  expect_equal(result_before, "03-08-2025")
  expect_equal(result_of, "03-09-2025")
  expect_equal(result_after, "03-10-2025")
})
