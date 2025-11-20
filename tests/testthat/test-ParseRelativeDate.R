library(lubridate)
# InitializePackage is called in helper-mock-data.R

# Reference dates for testing
ref_date_1 <- as.Date('2025-07-15')  # Mid-July
ref_date_2 <- as.Date('2025-10-09')  # Early October
ref_date_3 <- as.Date('2024-02-29')  # Leap day

# Helper to get today in Denver timezone
get_today <- function() {
  Sys.time() |>
    with_tz('America/Denver') |>
    as.Date()
}

test_that("'today' keyword returns current date", {
  today <- get_today()

  # Lowercase
  expect_equal(ParseRelativeDate('today'), today)
  expect_equal(ParseRelativeDate('today', type = 'start'), today)
  expect_equal(ParseRelativeDate('today', type = 'end'), today)

  # Uppercase
  expect_equal(ParseRelativeDate('TODAY'), today)
  expect_equal(ParseRelativeDate('TODAY', type = 'start'), today)
  expect_equal(ParseRelativeDate('TODAY', type = 'end'), today)
})

test_that("'CM' returns current month boundaries", {
  # Default (start of month)
  expect_equal(ParseRelativeDate('CM', refDate = ref_date_1), as.Date('2025-07-01'))
  expect_equal(ParseRelativeDate('CM', refDate = ref_date_2), as.Date('2025-10-01'))
  expect_equal(ParseRelativeDate('CM', refDate = ref_date_3), as.Date('2024-02-01'))

  # Explicit start
  expect_equal(ParseRelativeDate('CM', type = 'start', refDate = ref_date_1), as.Date('2025-07-01'))
  expect_equal(ParseRelativeDate('CM', type = 'start', refDate = ref_date_2), as.Date('2025-10-01'))
  expect_equal(ParseRelativeDate('CM', type = 'start', refDate = ref_date_3), as.Date('2024-02-01'))

  # End of month (including leap day)
  expect_equal(ParseRelativeDate('CM', type = 'end', refDate = ref_date_1), as.Date('2025-07-31'))
  expect_equal(ParseRelativeDate('CM', type = 'end', refDate = ref_date_2), as.Date('2025-10-31'))
  expect_equal(ParseRelativeDate('CM', type = 'end', refDate = ref_date_3), as.Date('2024-02-29'))
})

test_that("'CQ' returns current quarter boundaries", {
  # Q3 2025 (Jul-Sep)
  expect_equal(ParseRelativeDate('CQ', refDate = ref_date_1), as.Date('2025-07-01'))
  expect_equal(ParseRelativeDate('CQ', type = 'start', refDate = ref_date_1), as.Date('2025-07-01'))
  expect_equal(ParseRelativeDate('CQ', type = 'end', refDate = ref_date_1), as.Date('2025-09-30'))

  # Q4 2025 (Oct-Dec)
  expect_equal(ParseRelativeDate('CQ', refDate = ref_date_2), as.Date('2025-10-01'))
  expect_equal(ParseRelativeDate('CQ', type = 'start', refDate = ref_date_2), as.Date('2025-10-01'))
  expect_equal(ParseRelativeDate('CQ', type = 'end', refDate = ref_date_2), as.Date('2025-12-31'))

  # Q1 2024 (Jan-Mar, with leap day)
  expect_equal(ParseRelativeDate('CQ', refDate = ref_date_3), as.Date('2024-01-01'))
  expect_equal(ParseRelativeDate('CQ', type = 'start', refDate = ref_date_3), as.Date('2024-01-01'))
  expect_equal(ParseRelativeDate('CQ', type = 'end', refDate = ref_date_3), as.Date('2024-03-31'))
})

test_that("'CY' returns current year boundaries", {
  expect_equal(ParseRelativeDate('CY', refDate = ref_date_1), as.Date('2025-01-01'))
  expect_equal(ParseRelativeDate('CY', type = 'start', refDate = ref_date_1), as.Date('2025-01-01'))
  expect_equal(ParseRelativeDate('CY', type = 'end', refDate = ref_date_1), as.Date('2025-12-31'))

  expect_equal(ParseRelativeDate('CY', refDate = ref_date_2), as.Date('2025-01-01'))
  expect_equal(ParseRelativeDate('CY', type = 'start', refDate = ref_date_2), as.Date('2025-01-01'))
  expect_equal(ParseRelativeDate('CY', type = 'end', refDate = ref_date_2), as.Date('2025-12-31'))

  # Leap year
  expect_equal(ParseRelativeDate('CY', refDate = ref_date_3), as.Date('2024-01-01'))
  expect_equal(ParseRelativeDate('CY', type = 'start', refDate = ref_date_3), as.Date('2024-01-01'))
  expect_equal(ParseRelativeDate('CY', type = 'end', refDate = ref_date_3), as.Date('2024-12-31'))
})

test_that("Day offsets work correctly", {
  # Positive offsets (future)
  expect_equal(ParseRelativeDate('D+1'), get_today() + 1)
  expect_equal(ParseRelativeDate('D+7'), get_today() + 7)
  expect_equal(ParseRelativeDate('D+30'), get_today() + 30)

  # Negative offsets (past)
  expect_equal(ParseRelativeDate('D-1'), get_today() - 1)
  expect_equal(ParseRelativeDate('D-7'), get_today() - 7)
  expect_equal(ParseRelativeDate('D-30'), get_today() - 30)

  # With specific reference dates
  expect_equal(ParseRelativeDate('CM+5', refDate = ref_date_1), as.Date('2025-12-01'))
  expect_equal(ParseRelativeDate('CM-5', refDate = ref_date_1), as.Date('2025-02-01'))
  expect_equal(ParseRelativeDate('CM+5', type = 'end', refDate = ref_date_1), as.Date('2025-12-31'))
  expect_equal(ParseRelativeDate('CM-5', type = 'end', refDate = ref_date_1), as.Date('2025-02-28'))
})

test_that("'CQ' with offsets returns correct quarter boundaries", {
  # CQ+1 (next quarter)
  expect_equal(ParseRelativeDate('CQ+1', refDate = ref_date_1), as.Date('2025-10-01'))
  expect_equal(ParseRelativeDate('CQ+1', type = 'start', refDate = ref_date_1), as.Date('2025-10-01'))
  expect_equal(ParseRelativeDate('CQ+1', type = 'end', refDate = ref_date_1), as.Date('2025-12-31'))

  # CQ-1 (previous quarter)
  expect_equal(ParseRelativeDate('CQ-1', refDate = ref_date_1), as.Date('2025-04-01'))
  expect_equal(ParseRelativeDate('CQ-1', type = 'start', refDate = ref_date_1), as.Date('2025-04-01'))
  expect_equal(ParseRelativeDate('CQ-1', type = 'end', refDate = ref_date_1), as.Date('2025-06-30'))

  # CQ+2 (two quarters ahead)
  expect_equal(ParseRelativeDate('CQ+2', refDate = ref_date_3), as.Date('2024-07-01'))
  expect_equal(ParseRelativeDate('CQ+2', type = 'end', refDate = ref_date_3), as.Date('2024-09-30'))

  # CQ-2 (two quarters back)
  expect_equal(ParseRelativeDate('CQ-2', refDate = ref_date_2), as.Date('2025-04-01'))
  expect_equal(ParseRelativeDate('CQ-2', type = 'end', refDate = ref_date_2), as.Date('2025-06-30'))
})

test_that("'CM' with offsets returns correct month boundaries", {
  # CM+1 (next month)
  expect_equal(ParseRelativeDate('CM+1', refDate = ref_date_1), as.Date('2025-08-01'))
  expect_equal(ParseRelativeDate('CM+1', type = 'end', refDate = ref_date_1), as.Date('2025-08-31'))

  # CM-1 (previous month)
  expect_equal(ParseRelativeDate('CM-1', refDate = ref_date_1), as.Date('2025-06-01'))
  expect_equal(ParseRelativeDate('CM-1', type = 'end', refDate = ref_date_1), as.Date('2025-06-30'))

  # CM+3 (three months ahead)
  expect_equal(ParseRelativeDate('CM+3', refDate = ref_date_2), as.Date('2026-01-01'))
  expect_equal(ParseRelativeDate('CM+3', type = 'end', refDate = ref_date_2), as.Date('2026-01-31'))

  # CM-1 from leap day (handles February correctly)
  expect_equal(ParseRelativeDate('CM-1', refDate = ref_date_3), as.Date('2024-01-01'))
  expect_equal(ParseRelativeDate('CM-1', type = 'end', refDate = ref_date_3), as.Date('2024-01-31'))
})

test_that("'CY' with offsets returns correct year boundaries", {
  # CY+1 (next year)
  expect_equal(ParseRelativeDate('CY+1', refDate = ref_date_1), as.Date('2026-01-01'))
  expect_equal(ParseRelativeDate('CY+1', type = 'end', refDate = ref_date_1), as.Date('2026-12-31'))

  # CY-1 (previous year)
  expect_equal(ParseRelativeDate('CY-1', refDate = ref_date_1), as.Date('2024-01-01'))
  expect_equal(ParseRelativeDate('CY-1', type = 'end', refDate = ref_date_1), as.Date('2024-12-31'))

  # CY+2 from leap year
  expect_equal(ParseRelativeDate('CY+2', refDate = ref_date_3), as.Date('2026-01-01'))
  expect_equal(ParseRelativeDate('CY+2', type = 'end', refDate = ref_date_3), as.Date('2026-12-31'))
})

test_that("Case insensitivity works for all keywords", {
  expect_equal(ParseRelativeDate('cm', refDate = ref_date_1),
               ParseRelativeDate('CM', refDate = ref_date_1))
  expect_equal(ParseRelativeDate('cq', refDate = ref_date_1),
               ParseRelativeDate('CQ', refDate = ref_date_1))
  expect_equal(ParseRelativeDate('cy', refDate = ref_date_1),
               ParseRelativeDate('CY', refDate = ref_date_1))
  expect_equal(ParseRelativeDate('Cm+1', refDate = ref_date_1),
               ParseRelativeDate('CM+1', refDate = ref_date_1))
})

test_that("Invalid inputs are handled appropriately", {
  expect_error(ParseRelativeDate('invalid'))
  expect_error(ParseRelativeDate('CM+abc'))
  expect_error(ParseRelativeDate(''))
  expect_error(ParseRelativeDate(NULL))
})
