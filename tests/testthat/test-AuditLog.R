# Test Suite for AuditLog Function
# Run with: testthat::test_file("test_AuditLog.R")

library(testthat)
library(DBI)
library(RSQLite)

# Mock session object creator
createMockSession <- function(user = "testUser") {
  list(user = user)
}

# Test basic logging functionality
test_that("AuditLog writes to database correctly", {
  mockSession <- createMockSession("john.doe")

  # Get count before
  countBefore <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT COUNT(*) as n FROM audit_log"
  )$n

  # Execute
  result <- AuditLog("Viewed patient record", mockSession)

  # Verify
  expect_equal(result, 1)

  # Check last entry
  lastEntry <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT * FROM audit_log ORDER BY id DESC LIMIT 1"
  )
  expect_equal(nrow(lastEntry), 1)
  expect_equal(lastEntry$username, "john.doe")
  expect_equal(lastEntry$user_action, "Viewed patient record")
  expect_match(lastEntry$date_time, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")

  # Verify count increased by 1
  countAfter <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT COUNT(*) as n FROM audit_log"
  )$n
  expect_equal(countAfter, countBefore + 1)
})

# Test handling of NULL username
test_that("AuditLog handles NULL username correctly", {
  mockSession <- createMockSession(NULL)

  # Execute
  result <- AuditLog("Anonymous action", mockSession)

  # Verify last entry
  lastEntry <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT * FROM audit_log ORDER BY id DESC LIMIT 1"
  )
  expect_equal(lastEntry$username, "Unknown")
  expect_equal(lastEntry$user_action, "Anonymous action")
})

# Test multiple log entries
test_that("AuditLog handles multiple entries correctly", {
  mockSession <- createMockSession("jane.smith")

  # Get count before
  countBefore <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT COUNT(*) as n FROM audit_log"
  )$n

  # Execute multiple logs
  AuditLog("Action 1", mockSession)
  AuditLog("Action 2", mockSession)
  AuditLog("Action 3", mockSession)

  # Verify count increased by 3
  countAfter <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT COUNT(*) as n FROM audit_log"
  )$n
  expect_equal(countAfter, countBefore + 3)

  # Verify last 3 entries
  lastEntries <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT * FROM audit_log ORDER BY id DESC LIMIT 3"
  )
  expect_equal(nrow(lastEntries), 3)
  # Entries are in reverse order (newest first)
  expect_equal(lastEntries$user_action, c("Action 3", "Action 2", "Action 1"))
  expect_true(all(lastEntries$username == "jane.smith"))
})

# Test input validation
test_that("AuditLog validates inputs correctly", {
  mockSession <- createMockSession("test.user")

  # Test invalid userAction (not character)
  expect_error(
    AuditLog(123, mockSession),
    "'userAction' must be a single character string"
  )

  # Test invalid userAction (vector)
  expect_error(
    AuditLog(c("Action 1", "Action 2"), mockSession),
    "'userAction' must be a single character string"
  )

  # Test missing session
  expect_error(
    AuditLog("Some action"),
    "'session' must be provided"
  )

  # Test NULL session
  expect_error(
    AuditLog("Some action", NULL),
    "'session' must be provided"
  )
})

# Test SQL injection protection
test_that("AuditLog prevents SQL injection", {
  mockSession <- createMockSession("test'; DROP TABLE audit_log; --")

  # Execute with malicious input
  result <- AuditLog("Action with 'quotes' and \"double quotes\"", mockSession)

  # Verify table still exists and data is safely stored
  expect_true(dbExistsTable(.pkg_env$app_data$CON, "audit_log"))

  # Check last entry
  lastEntry <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT * FROM audit_log ORDER BY id DESC LIMIT 1"
  )
  expect_equal(nrow(lastEntry), 1)
  expect_equal(lastEntry$username, "test'; DROP TABLE audit_log; --")
  expect_equal(
    lastEntry$user_action,
    "Action with 'quotes' and \"double quotes\""
  )
})

# Test error handling
test_that("AuditLog handles database errors gracefully", {
  # Save original connection
  old_app_data <- .pkg_env$app_data

  # Create a disconnected connection to simulate error
  bad_con <- dbConnect(SQLite(), ":memory:")
  dbDisconnect(bad_con)
  .pkg_env$app_data <- list(CON = bad_con)

  mockSession <- createMockSession("test.user")

  # Execute and expect warning
  expect_warning(
    result <- AuditLog("Test action", mockSession),
    "Failed to write to audit log"
  )

  # Should return 0 on error
  expect_equal(result, 0)

  # Restore original connection
  .pkg_env$app_data <- old_app_data
})

# Test special characters
test_that("AuditLog handles special characters correctly", {
  mockSession <- createMockSession("user@domain.com")

  # Test with various special characters
  specialAction <- "Updated field: O'Brien's data with 100% accuracy & <tags>"
  result <- AuditLog(specialAction, mockSession)

  # Verify last entry
  lastEntry <- dbGetQuery(
    .pkg_env$app_data$CON,
    "SELECT * FROM audit_log ORDER BY id DESC LIMIT 1"
  )
  expect_equal(lastEntry$user_action, specialAction)
  expect_equal(lastEntry$username, "user@domain.com")
})
