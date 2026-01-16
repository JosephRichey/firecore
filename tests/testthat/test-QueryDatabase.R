# Test Suite for QueryDatabase Function
# Run with: testthat::test_file("test_QueryDatabase.R")

library(testthat)
library(DBI)
library(RSQLite)
library(logger)

# Setup test environment
setupTestDb <- function() {
  # Create in-memory SQLite database
  con <- dbConnect(SQLite(), ":memory:")

  # Create test tables
  dbExecute(
    con,
    "
    CREATE TABLE patients (
      id INTEGER PRIMARY KEY,
      name TEXT NOT NULL,
      age INTEGER
    )
  "
  )

  dbExecute(
    con,
    "
    CREATE TABLE medications (
      id INTEGER PRIMARY KEY,
      drug_name TEXT NOT NULL,
      dosage TEXT
    )
  "
  )

  # Insert test data
  dbExecute(
    con,
    "
    INSERT INTO patients (id, name, age) VALUES 
    (1, 'John Doe', 45),
    (2, 'Jane Smith', 32),
    (3, 'Bob Johnson', 67)
  "
  )

  dbExecute(
    con,
    "
    INSERT INTO medications (id, drug_name, dosage) VALUES 
    (1, 'Aspirin', '100mg'),
    (2, 'Ibuprofen', '200mg')
  "
  )

  return(con)
}

# Test basic query functionality
test_that("QueryDatabase retrieves data correctly", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Execute
  result <- QueryDatabase("patients")

  # Verify
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("id", "name", "age"))
  expect_equal(result$name, c("John Doe", "Jane Smith", "Bob Johnson"))

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test querying different tables
test_that("QueryDatabase works with multiple tables", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Execute
  patients <- QueryDatabase("patients")
  medications <- QueryDatabase("medications")

  # Verify
  expect_equal(nrow(patients), 3)
  expect_equal(nrow(medications), 2)
  expect_equal(medications$drug_name, c("Aspirin", "Ibuprofen"))

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test empty table
test_that("QueryDatabase handles empty tables correctly", {
  # Setup
  con <- setupTestDb()
  dbExecute(con, "CREATE TABLE empty_table (id INTEGER, value TEXT)")
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Execute
  result <- QueryDatabase("empty_table")

  # Verify
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test non-existent table
test_that("QueryDatabase handles non-existent tables gracefully", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Suppress log output for cleaner test results
  suppressMessages({
    log_threshold(FATAL)

    # Execute
    result <- QueryDatabase("nonexistent_table")

    # Reset log threshold
    log_threshold(INFO)
  })

  # Verify
  expect_null(result)

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test input validation
test_that("QueryDatabase validates input correctly", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Test non-character input
  expect_error(
    QueryDatabase(123),
    "'tableName' must be a single character string"
  )

  # Test vector input
  expect_error(
    QueryDatabase(c("table1", "table2")),
    "'tableName' must be a single character string"
  )

  # Test empty string
  expect_error(
    QueryDatabase(""),
    "'tableName' cannot be empty"
  )

  # Test invalid characters (SQL injection attempt)
  expect_error(
    QueryDatabase("patients; DROP TABLE patients; --"),
    "'tableName' contains invalid characters"
  )

  # Test invalid characters (spaces)
  expect_error(
    QueryDatabase("my table"),
    "'tableName' contains invalid characters"
  )

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test valid special characters in table names
test_that("QueryDatabase accepts valid special characters", {
  # Setup
  con <- setupTestDb()
  dbExecute(con, "CREATE TABLE test_table_123 (id INTEGER)")
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Test underscore
  result1 <- QueryDatabase("test_table_123")
  expect_false(is.null(result1))

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test schema-qualified table names
test_that("QueryDatabase handles schema.table notation", {
  # Setup
  con <- setupTestDb()
  # SQLite doesn't have schemas, but we can test the quoting logic
  dbExecute(con, "CREATE TABLE main.test_schema_table (id INTEGER, value TEXT)")
  dbExecute(con, "INSERT INTO main.test_schema_table VALUES (1, 'test')")
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Test schema.table notation
  result <- QueryDatabase("main.test_schema_table")
  expect_false(is.null(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$value, "test")

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test invalid schema.table format
test_that("QueryDatabase rejects invalid schema.table formats", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Test too many periods
  expect_error(
    QueryDatabase("schema.table.extra"),
    "'tableName' must be in format 'table' or 'schema.table'"
  )

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test missing connection
test_that("QueryDatabase handles missing connection gracefully", {
  # Remove app_data if it exists
  if (exists("app_data", envir = .GlobalEnv)) {
    rm(app_data, envir = .GlobalEnv)
  }

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute and expect error
    expect_error(
      QueryDatabase("patients"),
      "Database connection.*not available"
    )

    # Reset log threshold
    log_threshold(INFO)
  })
})

# Test disconnected connection
test_that("QueryDatabase handles disconnected connection", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Disconnect the connection
  dbDisconnect(con)

  # Suppress log output
  suppressMessages({
    log_threshold(FATAL)

    # Execute
    result <- QueryDatabase("patients")

    # Reset log threshold
    log_threshold(INFO)
  })

  # Verify
  expect_null(result)

  # Cleanup
  rm(app_data, envir = .GlobalEnv)
})

# Test data integrity
test_that("QueryDatabase preserves data types correctly", {
  # Setup
  con <- setupTestDb()
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Execute
  result <- QueryDatabase("patients")

  # Verify data types
  expect_type(result$id, "integer")
  expect_type(result$name, "character")
  expect_type(result$age, "integer")

  # Verify specific values
  expect_equal(result$age[1], 45)
  expect_equal(result$age[2], 32)
  expect_equal(result$age[3], 67)

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})

# Test with special characters in data
test_that("QueryDatabase handles special characters in data", {
  # Setup
  con <- setupTestDb()
  dbExecute(
    con,
    "
    INSERT INTO patients (id, name, age) VALUES 
    (4, 'O''Brien', 50),
    (5, 'Smith & Jones', 40)
  "
  )
  app_data <- list(CON = con)
  assign("app_data", app_data, envir = .GlobalEnv)

  # Execute
  result <- QueryDatabase("patients")

  # Verify
  expect_equal(nrow(result), 5)
  expect_true("O'Brien" %in% result$name)
  expect_true("Smith & Jones" %in% result$name)

  # Cleanup
  dbDisconnect(con)
  rm(app_data, envir = .GlobalEnv)
})
