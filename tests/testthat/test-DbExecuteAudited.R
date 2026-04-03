# mockery is a Suggests dependency — load it here for this test file
if (!requireNamespace("mockery", quietly = TRUE)) {
  skip("mockery package not available")
}
library(mockery)

# --- Test helper ---
stub_externals <- function(db_result = 1L, db_error = NULL) {
  stub(DbExecuteAudited, ".CheckPackageEnv", invisible(NULL))
  stub(DbExecuteAudited, ".pkg_env$app_data", list(CON = "mock_con"))
  stub(DbExecuteAudited, "AuditLog", invisible(NULL))
  stub(DbExecuteAudited, "NotifyError", invisible(NULL))
  stub(DbExecuteAudited, "CheckWriteResult", TRUE)
  stub(DbExecuteAudited, "logger::log_info", invisible(NULL))
  stub(DbExecuteAudited, "logger::log_error", invisible(NULL))

  if (!is.null(db_error)) {
    stub(DbExecuteAudited, "DBI::dbExecute", function(...) stop(db_error))
  } else {
    stub(DbExecuteAudited, "DBI::dbExecute", db_result)
  }
}

SQL <- "UPDATE firefighter SET is_active = 1 WHERE id = 1"


# --- Input validation ---

test_that("stops when sql is NULL", {
  stub(DbExecuteAudited, ".CheckPackageEnv", invisible(NULL))
  stub(DbExecuteAudited, ".pkg_env$app_data", list(CON = "mock_con"))

  expect_error(
    DbExecuteAudited(sql = NULL, currentUser = NULL),
    "'sql' must be a non-empty character string or DBI SQL object"
  )
})

test_that("stops when sql is blank", {
  stub(DbExecuteAudited, ".CheckPackageEnv", invisible(NULL))
  stub(DbExecuteAudited, ".pkg_env$app_data", list(CON = "mock_con"))

  expect_error(
    DbExecuteAudited(sql = "   ", currentUser = NULL),
    "'sql' must be a non-empty character string or DBI SQL object"
  )
})

test_that("stops when expectedMin > expectedMax", {
  stub(DbExecuteAudited, ".CheckPackageEnv", invisible(NULL))
  stub(DbExecuteAudited, ".pkg_env$app_data", list(CON = "mock_con"))

  expect_error(
    DbExecuteAudited(SQL, currentUser = NULL, expectedMin = 5, expectedMax = 1),
    "'expectedMin' must be less than or equal to 'expectedMax'"
  )
})


# --- Return values ---

test_that("returns TRUE on successful single-row execute", {
  stub_externals(db_result = 1L)
  stub(DbExecuteAudited, "CheckWriteResult", TRUE)

  result <- DbExecuteAudited(
    SQL,
    currentUser = NULL,
    context = "updating a firefighter"
  )
  expect_true(result)
})

test_that("returns FALSE when 0 rows affected and expectedMin = 1", {
  stub_externals(db_result = 0L)
  stub(DbExecuteAudited, "CheckWriteResult", FALSE)

  result <- DbExecuteAudited(
    SQL,
    currentUser = NULL,
    context = "updating a firefighter"
  )
  expect_false(result)
})

test_that("returns TRUE when 0 rows affected and expectedMin = 0", {
  stub_externals(db_result = 0L)
  stub(DbExecuteAudited, "CheckWriteResult", TRUE)

  result <- DbExecuteAudited(
    "DELETE FROM audit_log WHERE date < '2020-01-01'",
    currentUser = NULL,
    context = "pruning old audit logs",
    expectedMin = 0,
    expectedMax = 1000
  )
  expect_true(result)
})

test_that("returns FALSE and does not throw when dbExecute errors", {
  stub_externals(db_error = "Connection timed out")
  stub(DbExecuteAudited, "CheckWriteResult", FALSE)

  result <- NULL
  expect_no_error(
    result <- DbExecuteAudited(
      SQL,
      currentUser = NULL,
      context = "updating a firefighter"
    )
  )
  expect_false(result)
})
