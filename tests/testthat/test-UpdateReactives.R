# Test Suite for UpdateReactives Function
# Run with: testthat::test_file("test_UpdateReactives.R")

library(testthat)
library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(logger)
library(withr)

# Enable reactive console for testing
reactiveConsole(TRUE)

# Mock ConvertToLocalPosix for testing
ConvertToLocalPosix <- function(x, input, output) {
  # Simple mock that just returns the input
  return(x)
}

# Setup test database
setupTestDb <- function() {
  con <- dbConnect(SQLite(), ":memory:")

  # Create training table
  dbExecute(
    con,
    "
    CREATE TABLE training (
      id INTEGER PRIMARY KEY,
      start_time TEXT,
      end_time TEXT,
      training_type TEXT
    )
  "
  )

  dbExecute(
    con,
    "
    INSERT INTO training VALUES 
    (1, '2024-01-15 10:00:00', '2024-01-15 12:00:00', 'CPR'),
    (2, '2024-01-16 14:00:00', '2024-01-16 16:00:00', 'Fire Suppression')
  "
  )

  # Create firefighter table
  dbExecute(
    con,
    "
    CREATE TABLE firefighter (
      id INTEGER PRIMARY KEY,
      name TEXT,
      display_order INTEGER,
      start_date TEXT
    )
  "
  )

  dbExecute(
    con,
    "
    INSERT INTO firefighter VALUES 
    (1, 'John Doe', 2, '2020-01-15'),
    (2, 'Jane Smith', 1, '2019-06-01'),
    (3, 'Bob Johnson', 3, '2021-03-10')
  "
  )

  # Create attendance table
  dbExecute(
    con,
    "
    CREATE TABLE attendance (
      id INTEGER PRIMARY KEY,
      firefighter_id INTEGER,
      check_in TEXT,
      check_out TEXT
    )
  "
  )

  dbExecute(
    con,
    "
    INSERT INTO attendance VALUES 
    (1, 1, '2024-01-15 08:00:00', '2024-01-15 17:00:00'),
    (2, 2, '2024-01-15 08:00:00', '2024-01-15 17:00:00')
  "
  )

  # Create firefighter_response table
  dbExecute(
    con,
    "
    CREATE TABLE firefighter_response (
      id INTEGER PRIMARY KEY,
      firefighter_id INTEGER,
      incident_id INTEGER,
      time_adjustment REAL
    )
  "
  )

  dbExecute(
    con,
    "
    INSERT INTO firefighter_response VALUES 
    (1, 1, 100, 5.5),
    (2, 2, 100, NULL),
    (3, 1, 101, 0)
  "
  )

  return(con)
}

# Test basic functionality with options
test_that("UpdateReactives uses options when dbTableName is NULL", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Set options
  local_options(list(
    firecore.default_tables = c('training', 'firefighter')
  ))

  rdfs <- reactiveValues()

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute
    UpdateReactives(rdfs)

    log_threshold(INFO)
  })

  # Verify
  expect_true("training" %in% names(reactiveValuesToList(rdfs)))
  expect_true("firefighter" %in% names(reactiveValuesToList(rdfs)))
  expect_equal(nrow(rdfs$training), 2)
  expect_equal(nrow(rdfs$firefighter), 3)

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test explicit dbTableName overrides options
test_that("UpdateReactives explicit dbTableName overrides options", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Set options (should be ignored)
  local_options(list(
    firecore.default_tables = c('training', 'firefighter')
  ))

  rdfs <- reactiveValues()

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute with explicit tables
    UpdateReactives(rdfs, dbTableName = c('attendance'))

    log_threshold(INFO)
  })

  # Verify only attendance was loaded
  vals <- reactiveValuesToList(rdfs)
  expect_true("attendance" %in% names(vals))
  expect_false("training" %in% names(vals))
  expect_false("firefighter" %in% names(vals))

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test error when no tables specified
test_that("UpdateReactives errors when no tables specified", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Clear any existing options
  local_options(list(firecore.default_tables = NULL))

  rdfs <- reactiveValues()

  # Execute and expect error
  expect_error(
    UpdateReactives(rdfs),
    "must be specified or set via options"
  )

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test firefighter table sorting
test_that("UpdateReactives sorts firefighter by display_order", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  rdfs <- reactiveValues()

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute
    UpdateReactives(rdfs, dbTableName = 'firefighter')

    log_threshold(INFO)
  })

  # Verify sorting
  expect_equal(rdfs$firefighter$display_order, c(1, 2, 3))
  expect_equal(
    rdfs$firefighter$name,
    c('Jane Smith', 'John Doe', 'Bob Johnson')
  )

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test firefighter_response NA handling
test_that("UpdateReactives handles NA time_adjustment in firefighter_response", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  rdfs <- reactiveValues()

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute
    UpdateReactives(rdfs, dbTableName = 'firefighter_response')

    log_threshold(INFO)
  })

  # Verify NA replaced with 0
  expect_equal(rdfs$firefighter_response$time_adjustment, c(5.5, 0, 0))
  expect_false(any(is.na(rdfs$firefighter_response$time_adjustment)))

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test multiple tables
test_that("UpdateReactives handles multiple tables", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  rdfs <- reactiveValues()

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute
    UpdateReactives(
      rdfs,
      dbTableName = c('training', 'attendance', 'firefighter')
    )

    log_threshold(INFO)
  })

  # Verify all tables loaded
  vals <- reactiveValuesToList(rdfs)
  expect_true("training" %in% names(vals))
  expect_true("attendance" %in% names(vals))
  expect_true("firefighter" %in% names(vals))
  expect_equal(nrow(rdfs$training), 2)
  expect_equal(nrow(rdfs$attendance), 2)
  expect_equal(nrow(rdfs$firefighter), 3)

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test unknown table (no transformation)
test_that("UpdateReactives handles tables without specific transformations", {
  # Setup
  con <- setupTestDb()
  dbExecute(con, "CREATE TABLE unknown_table (id INTEGER, value TEXT)")
  dbExecute(con, "INSERT INTO unknown_table VALUES (1, 'test')")
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  rdfs <- reactiveValues()

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute
    UpdateReactives(rdfs, dbTableName = 'unknown_table')

    log_threshold(INFO)
  })

  # Verify table loaded without transformation
  expect_equal(nrow(rdfs$unknown_table), 1)
  expect_equal(rdfs$unknown_table$value, 'test')

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test input validation
test_that("UpdateReactives validates inputs", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Test missing rdfs
  expect_error(
    UpdateReactives(),
    "'rdfs' must be a reactivevalues object"
  )

  # Test wrong type for rdfs
  expect_error(
    UpdateReactives(list()),
    "'rdfs' must be a reactivevalues object"
  )

  # Test empty dbTableName
  rdfs <- reactiveValues()
  expect_error(
    UpdateReactives(rdfs, dbTableName = character(0)),
    "'dbTableName' must be a non-empty character vector"
  )

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test failed query handling
test_that("UpdateReactives skips tables that fail to query", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  rdfs <- reactiveValues()

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute with mix of valid and invalid tables
    UpdateReactives(
      rdfs,
      dbTableName = c('training', 'nonexistent', 'firefighter')
    )

    log_threshold(INFO)
  })

  # Verify valid tables loaded, invalid skipped
  vals <- reactiveValuesToList(rdfs)
  expect_true("training" %in% names(vals))
  expect_true("firefighter" %in% names(vals))
  expect_false("nonexistent" %in% names(vals))

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})
