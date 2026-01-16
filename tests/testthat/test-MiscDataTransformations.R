# ============================================================================
# test_BuildNamedVector.R
# ============================================================================

library(testthat)
library(dplyr)
library(rlang)
library(stringr)


test_that("BuildNamedVector creates basic named vector", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob")
  )

  result <- BuildNamedVector(df, name, id)

  expect_named(result, c("John", "Jane", "Bob"))
  expect_equal(as.numeric(result), c(1, 2, 3))
})

test_that("BuildNamedVector applies filter correctly", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    status = c("active", "inactive", "active")
  )

  result <- BuildNamedVector(df, name, id, status == "active")

  expect_named(result, c("John", "Bob"))
  expect_equal(as.numeric(result), c(1, 3))
})

test_that("BuildNamedVector works with different column types", {
  df <- data.frame(
    id = c("A", "B", "C"),
    name = c("Alpha", "Beta", "Gamma"),
    value = c(10, 20, 30)
  )

  result <- BuildNamedVector(df, name, id)

  expect_named(result, c("Alpha", "Beta", "Gamma"))
  expect_equal(result, c("Alpha" = "A", "Beta" = "B", "Gamma" = "C"))
})

test_that("BuildNamedVector handles empty data frame", {
  df <- data.frame(id = integer(), name = character())

  expect_warning(
    result <- BuildNamedVector(df, name, id),
    "empty"
  )

  expect_length(result, 0)
})

test_that("BuildNamedVector handles filter resulting in empty set", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    status = c("active", "active", "active")
  )

  expect_warning(
    result <- BuildNamedVector(df, name, id, status == "inactive"),
    "empty data frame"
  )

  expect_length(result, 0)
})

test_that("BuildNamedVector validates input", {
  expect_error(
    BuildNamedVector("not a df", name, id),
    "'df' must be a data frame"
  )
})

test_that("BuildNamedVector works without filter", {
  df <- data.frame(
    id = 1:2,
    name = c("Test1", "Test2")
  )

  result <- BuildNamedVector(df, name, id)

  expect_length(result, 2)
})

# ============================================================================
# test_IdToString.R
# ============================================================================

test_that("IdToString retrieves correct value", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    rank = c("Captain", "Lieutenant", "Firefighter")
  )

  result <- IdToString(df, name, 2)

  expect_equal(result, "Jane")
})

test_that("IdToString works with different columns", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    rank = c("Captain", "Lieutenant", "Firefighter")
  )

  result <- IdToString(df, rank, 1)

  expect_equal(result, "Captain")
})

test_that("IdToString returns empty vector for non-existent ID", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob")
  )

  result <- IdToString(df, name, 99)

  expect_length(result, 0)
})

test_that("IdToString works with single row result", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob")
  )

  result <- IdToString(df, name, 1)

  expect_equal(result, "John")
  expect_length(result, 1)
})

test_that("IdToString validates input", {
  df <- data.frame(name = c("John", "Jane"))

  expect_error(
    IdToString(df, name, 1),
    "'df' must contain an 'id' column"
  )

  expect_error(
    IdToString("not a df", name, 1),
    "'df' must be a data frame"
  )
})

test_that("IdToString works with numeric IDs", {
  df <- data.frame(
    id = c(100, 200, 300),
    name = c("Alpha", "Beta", "Gamma")
  )

  result <- IdToString(df, name, 200)

  expect_equal(result, "Beta")
})

# ============================================================================
# test_StringToId.R
# ============================================================================

test_that("StringToId retrieves correct ID", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob")
  )

  result <- StringToId(df, name, "Jane")

  expect_equal(result, 2)
})

test_that("StringToId works with different columns", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob"),
    rank = c("Captain", "Lieutenant", "Firefighter")
  )

  result <- StringToId(df, rank, "Captain")

  expect_equal(result, 1)
})

test_that("StringToId returns empty vector for non-existent value", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob")
  )

  result <- StringToId(df, name, "Unknown")

  expect_length(result, 0)
})

test_that("StringToId works with single row result", {
  df <- data.frame(
    id = 1:3,
    name = c("John", "Jane", "Bob")
  )

  result <- StringToId(df, name, "John")

  expect_equal(result, 1)
  expect_length(result, 1)
})

test_that("StringToId validates input", {
  df <- data.frame(name = c("John", "Jane"))

  expect_error(
    StringToId(df, name, "John"),
    "'df' must contain an 'id' column"
  )

  expect_error(
    StringToId("not a df", name, "value"),
    "'df' must be a data frame"
  )

  df_with_id <- data.frame(id = 1:2, name = c("John", "Jane"))
  expect_error(
    StringToId(df_with_id, name),
    "'value' must be provided"
  )
})

test_that("StringToId works with case-sensitive matching", {
  df <- data.frame(
    id = 1:3,
    name = c("john", "Jane", "BOB")
  )

  result1 <- StringToId(df, name, "john")
  result2 <- StringToId(df, name, "John")

  expect_equal(result1, 1)
  expect_length(result2, 0)
})

test_that("StringToId works with numeric values", {
  df <- data.frame(
    id = 1:3,
    code = c(100, 200, 300)
  )

  result <- StringToId(df, code, 200)

  expect_equal(result, 2)
})

# ============================================================================
# test_FixColNames.R
# ============================================================================

test_that("FixColNames converts snake_case to Title Case", {
  df <- data.frame(
    first_name = "John",
    last_name = "Doe",
    hire_date = "2020-01-15"
  )

  result <- FixColNames(df)

  expect_equal(colnames(result), c("First Name", "Last Name", "Hire Date"))
})

test_that("FixColNames removes prefix correctly", {
  df <- data.frame(
    firefighter_id = 1,
    firefighter_name = "John",
    firefighter_rank = "Captain"
  )

  result <- FixColNames(df, prefix = "Firefighter ")

  expect_equal(colnames(result), c("Id", "Name", "Rank"))
})

test_that("FixColNames handles columns without underscores", {
  df <- data.frame(
    id = 1,
    name = "John",
    rank = "Captain"
  )

  result <- FixColNames(df)

  expect_equal(colnames(result), c("Id", "Name", "Rank"))
})

test_that("FixColNames handles multiple underscores", {
  df <- data.frame(
    very_long_column_name = 1,
    another_long_name = 2
  )

  result <- FixColNames(df)

  expect_equal(
    colnames(result),
    c("Very Long Column Name", "Another Long Name")
  )
})

test_that("FixColNames preserves data", {
  df <- data.frame(
    first_name = c("John", "Jane"),
    last_name = c("Doe", "Smith"),
    age = c(30, 25)
  )

  result <- FixColNames(df)

  expect_equal(result$`First Name`, c("John", "Jane"))
  expect_equal(result$`Last Name`, c("Doe", "Smith"))
  expect_equal(result$Age, c(30, 25))
})

test_that("FixColNames validates input", {
  expect_error(
    FixColNames("not a df"),
    "'data' must be a data frame"
  )

  df <- data.frame(x = 1)
  expect_error(
    FixColNames(df, prefix = c("A", "B")),
    "'prefix' must be NULL or a single character string"
  )
})

test_that("FixColNames handles empty data frame", {
  df <- data.frame()

  result <- FixColNames(df)

  expect_equal(ncol(result), 0)
})

test_that("FixColNames with prefix removes all instances", {
  df <- data.frame(
    user_id = 1,
    user_name = "John",
    user_status = "active"
  )

  result <- FixColNames(df, prefix = "User ")

  expect_equal(colnames(result), c("Id", "Name", "Status"))
})
