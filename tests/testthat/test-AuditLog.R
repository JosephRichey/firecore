# Test Suite for AuditLog Function
# Run with: testthat::test_file("test_AuditLog.R")

library(testthat)
library(DBI)
library(RSQLite)

# Setup test environment
setupTestDb <- function() {
  # Create in-memory SQLite database
  con <- dbConnect(SQLite(), ":memory:")

  # Create audit_log table
  dbExecute(
    con,
    "
    CREATE TABLE audit_log (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT NOT NULL,
      date_time TEXT NOT NULL,
      user_action TEXT NOT NULL
    )
  "
  )

  return(con)
}

# Mock session object creator
createMockSession <- function(user = "testUser") {
  list(user = user)
}

# Test basic logging functionality
test_that("AuditLog writes to database correctly", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  mockSession <- createMockSession("john.doe")

  # Execute
  result <- AuditLog("Viewed patient record", mockSession)

  # Verify
  expect_equal(result, 1)

  logEntries <- dbGetQuery(con, "SELECT * FROM audit_log")
  expect_equal(nrow(logEntries), 1)
  expect_equal(logEntries$username, "john.doe")
  expect_equal(logEntries$user_action, "Viewed patient record")
  expect_match(
    logEntries$date_time,
    "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"
  )

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test handling of NULL username
test_that("AuditLog handles NULL username correctly", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  mockSession <- createMockSession(NULL)

  # Execute
  result <- AuditLog("Anonymous action", mockSession)

  # Verify
  logEntries <- dbGetQuery(con, "SELECT * FROM audit_log")
  expect_equal(logEntries$username, "Unknown")
  expect_equal(logEntries$user_action, "Anonymous action")

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test multiple log entries
test_that("AuditLog handles multiple entries correctly", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  mockSession <- createMockSession("jane.smith")

  # Execute multiple logs
  AuditLog("Action 1", mockSession)
  AuditLog("Action 2", mockSession)
  AuditLog("Action 3", mockSession)

  # Verify
  logEntries <- dbGetQuery(con, "SELECT * FROM audit_log ORDER BY id")
  expect_equal(nrow(logEntries), 3)
  expect_equal(logEntries$user_action, c("Action 1", "Action 2", "Action 3"))
  expect_true(all(logEntries$username == "jane.smith"))

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test input validation
test_that("AuditLog validates inputs correctly", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

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

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test SQL injection protection
test_that("AuditLog prevents SQL injection", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  mockSession <- createMockSession("test'; DROP TABLE audit_log; --")

  # Execute with malicious input
  result <- AuditLog("Action with 'quotes' and \"double quotes\"", mockSession)

  # Verify table still exists and data is safely stored
  expect_true(dbExistsTable(con, "audit_log"))
  logEntries <- dbGetQuery(con, "SELECT * FROM audit_log")
  expect_equal(nrow(logEntries), 1)
  expect_equal(logEntries$username, "test'; DROP TABLE audit_log; --")
  expect_equal(
    logEntries$user_action,
    "Action with 'quotes' and \"double quotes\""
  )

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test error handling
test_that("AuditLog handles database errors gracefully", {
  # Setup with invalid connection
  con <- setupTestDb()
  dbDisconnect(con) # Disconnect to simulate error
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  mockSession <- createMockSession("test.user")

  # Execute and expect warning
  expect_warning(
    result <- AuditLog("Test action", mockSession),
    "Failed to write to audit log"
  )

  # Should return 0 on error
  expect_equal(result, 0)

  # Cleanup
  rm(app_data, envir = .GlobalEnv)
})

# Test special characters
test_that("AuditLog handles special characters correctly", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  mockSession <- createMockSession("user@domain.com")

  # Test with various special characters
  specialAction <- "Updated field: O'Brien's data with 100% accuracy & <tags>"
  result <- AuditLog(specialAction, mockSession)

  # Verify
  logEntries <- dbGetQuery(con, "SELECT * FROM audit_log")
  expect_equal(logEntries$user_action, specialAction)

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})
