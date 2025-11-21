
test_that("GetSetting returns string values correctly", {
  result <- GetSetting("global", key = "password")
  expect_equal(result, "123")
  expect_type(result, "character")

  result <- GetSetting("global", key = "ltz")
  expect_equal(result, "America/Denver")
  expect_type(result, "character")

  result <- GetSetting("incident", key = "incident_cad_regex")
  expect_equal(result, "(?:\\d[7])")
  expect_type(result, "character")
})

test_that("GetSetting converts boolean values correctly", {
  # TRUE values
  result <- GetSetting("global", key = "sidebar_open")
  expect_true(result)
  expect_type(result, "logical")

  result <- GetSetting("incident", key = "canceled")
  expect_true(result)
  expect_type(result, "logical")

  # FALSE values
  result <- GetSetting("global", key = "safe_delete")
  expect_false(result)
  expect_type(result, "logical")

  result <- GetSetting("incident", key = "input_secondary")
  expect_false(result)
  expect_type(result, "logical")
})

test_that("GetSetting filters by domain only", {
  # Get all global settings (should return multiple values)
  result <- GetSetting("global")
  expect_length(result, 7)
  expect_type(result, "character")

  # Get all incident settings
  result <- GetSetting("incident")
  expect_length(result, 6)
})

test_that("GetSetting filters by domain and group", {
  # Get all time-related settings
  result <- GetSetting("global", group = "time")
  expect_length(result, 3)
  expect_equal(result, c("America/Denver", "%m-%d-%Y", "%m-%d-%Y %H:%M"))

  # Get all incident_response settings
  result <- GetSetting("incident", group = "incident_response")
  expect_length(result, 6)
})

test_that("GetSetting filters by domain and key", {
  result <- GetSetting("global", key = "fire_dept_id")
  expect_equal(result, "Crabaple")
  expect_length(result, 1)

  result <- GetSetting("incident", key = "address")
  expect_true(result)
  expect_type(result, "logical")
})

test_that("GetSetting returns single value when one match", {
  result <- GetSetting("global", key = "password")
  expect_length(result, 1)
  expect_false(is.list(result))
  expect_equal(result, "123")
})

test_that("GetSetting returns vector when multiple matches", {
  result <- GetSetting("global", group = "time")
  expect_length(result, 3)
  expect_true(is.vector(result))
  expect_false(is.list(result))
})

test_that("GetSetting returns NA for non-existent domain", {
  expect_warning(
    result <- GetSetting("nonexistent", key = "fake"),
    NA  # Expecting no warning from the function itself, but log_error might
  )
  expect_true(is.na(result))
})

test_that("GetSetting returns NA for non-existent key", {
  result <- GetSetting("global", key = "does_not_exist")
  expect_true(is.na(result))
})

test_that("GetSetting returns NA for non-existent group", {
  result <- GetSetting("global", group = "fake_group")
  expect_true(is.na(result))
})

test_that("GetSetting returns NA for valid domain but invalid key", {
  result <- GetSetting("incident", key = "not_a_real_key")
  expect_true(is.na(result))
})

test_that("GetSetting handles numeric type conversion", {
  result <- GetSetting("test", key = "max_count")
  expect_equal(result, 42)
  expect_type(result, "double")
})

test_that("GetSetting handles date type conversion", {
  result <- GetSetting("test", key = "start_date")
  expect_s3_class(result, "Date")
  expect_equal(result, as.Date("2025-01-15"))
})

test_that("GetSetting handles boolean variations", {
  # TRUE variations
  expect_true(GetSetting("test", key = "bool1"))
  expect_true(GetSetting("test", key = "bool2"))
  expect_true(GetSetting("test", key = "bool3"))
  expect_true(GetSetting("test", key = "bool4"))

  # FALSE variations
  expect_false(GetSetting("test", key = "bool5"))
  expect_false(GetSetting("test", key = "bool6"))
})

test_that("GetSetting uses fallback coercion for unknown types", {
  result <- GetSetting("test", key = "unknown_type")
  expect_type(result, "character")
  expect_equal(result, "some_value")
})

test_that("GetSetting handles relative_date type", {
  result <- GetSetting("test", key = "relative")
  expect_s3_class(result, "Date")
  # Should be today's date
  expect_equal(result, Sys.time() |> with_tz('America/Denver') |> as.Date())
})

test_that("GetSetting with both key and group returns correct subset", {
  # Get time settings with specific key
  result <- GetSetting("global", key = "ltz", group = "time")
  expect_equal(result, "America/Denver")
  expect_length(result, 1)
})

test_that("GetSetting handles mixed types in same group", {
  # All incident_response settings (mix of booleans)
  result <- GetSetting("incident", group = "incident_response")
  expect_length(result, 6)
  # All should be coerced to same type (boolean for first entry)
  expect_type(result, "logical")
})

