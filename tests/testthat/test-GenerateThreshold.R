# Test Suite for GenerateThreshold Function
# Run with: testthat::test_file("test_GenerateThreshold.R")

library(testthat)
library(lubridate)
library(dplyr)

# Test basic day calculations
test_that("GenerateThreshold calculates days correctly", {
  baseDate <- as.Date("2024-01-15")

  # Subtract days (default)
  expect_equal(
    GenerateThreshold(baseDate, 10, "day"),
    as.Date("2024-01-05")
  )

  # Add days (expireCalc = TRUE)
  expect_equal(
    GenerateThreshold(baseDate, 10, "day", expireCalc = TRUE),
    as.Date("2024-01-25")
  )

  # Zero days
  expect_equal(
    GenerateThreshold(baseDate, 0, "day"),
    baseDate
  )
})

# Test month calculations
test_that("GenerateThreshold calculates months correctly", {
  baseDate <- as.Date("2024-01-15")

  # Subtract months
  expect_equal(
    GenerateThreshold(baseDate, 3, "month"),
    as.Date("2023-10-15")
  )

  # Add months
  expect_equal(
    GenerateThreshold(baseDate, 6, "month", expireCalc = TRUE),
    as.Date("2024-07-15")
  )

  # End of month handling
  endOfJan <- as.Date("2024-01-31")
  expect_equal(
    GenerateThreshold(endOfJan, 1, "month", expireCalc = TRUE),
    as.Date("2024-02-29") # Leap year
  )
})

# Test year calculations
test_that("GenerateThreshold calculates years correctly", {
  baseDate <- as.Date("2024-01-15")

  # Subtract years
  expect_equal(
    GenerateThreshold(baseDate, 2, "year"),
    as.Date("2022-01-15")
  )

  # Add years
  expect_equal(
    GenerateThreshold(baseDate, 5, "year", expireCalc = TRUE),
    as.Date("2029-01-15")
  )

  # Leap year edge case
  leapDay <- as.Date("2024-02-29")
  expect_equal(
    GenerateThreshold(leapDay, 1, "year"),
    as.Date("2023-02-28") # Non-leap year
  )
})

# Test vectorized operations
test_that("GenerateThreshold works with vector inputs", {
  dates <- as.Date(c("2024-01-15", "2024-02-20", "2024-03-25"))

  results <- GenerateThreshold(dates, 10, "day")
  expected <- as.Date(c("2024-01-05", "2024-02-10", "2024-03-15"))

  expect_equal(results, expected)
})

# Test input validation
test_that("GenerateThreshold validates inputs correctly", {
  validDate <- as.Date("2024-01-15")

  # Invalid date type
  expect_error(
    GenerateThreshold("2024-01-15", 10, "day"),
    "'date' must be a Date object"
  )

  # Negative leadTime
  expect_error(
    GenerateThreshold(validDate, -5, "day"),
    "'leadTime' must be a non-negative numeric value"
  )

  # Invalid leadTimeUnit
  expect_error(
    GenerateThreshold(validDate, 10, "week"),
    "'leadTimeUnit' must be one of"
  )

  # Invalid expireCalc type
  expect_error(
    GenerateThreshold(validDate, 10, "day", expireCalc = "yes"),
    "'expireCalc' must be TRUE or FALSE"
  )
})

# Test edge cases
test_that("GenerateThreshold handles edge cases", {
  # Very large lead times
  baseDate <- as.Date("2024-01-15")
  expect_equal(
    GenerateThreshold(baseDate, 1000, "day"),
    baseDate - days(1000)
  )

  # End of month consistency
  jan31 <- as.Date("2024-01-31")
  expect_equal(
    GenerateThreshold(jan31, 1, "month"),
    as.Date("2023-12-31")
  )

  # Year spanning
  expect_equal(
    GenerateThreshold(as.Date("2024-03-15"), 6, "month"),
    as.Date("2023-09-15")
  )
})
