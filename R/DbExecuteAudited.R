#' Execute a SQL Statement with Audit Logging
#'
#' Combines `DBI::dbExecute()`, `AuditLog()`, and `CheckWriteResult()` into a
#' single call. Handles DB errors internally — logs them, sends an error
#' notification, and returns `FALSE` — so callers can accumulate results without
#' wrapping every write in a `tryCatch`.
#'
#' For transactional operations, call this function inside a `tryCatch` block
#' that manages `dbBegin()` / `dbCommit()` / `dbRollback()`. A `FALSE` return
#' from inside a transaction should trigger a rollback in the caller; this
#' function does not roll back automatically.
#'
#' @param sql A character string or DBI SQL object containing the
#'   already-interpolated SQL statement to execute. Callers are responsible for
#'   parameterizing the query via `DBI::sqlInterpolate()` before passing it in.
#' @param currentUser A `reactiveVal`, `reactiveValues`, or plain character
#'   string identifying the user performing the action. Passed directly to
#'   `AuditLog()`.
#' @param context Optional character string describing the operation, used in
#'   log messages and the error modal (e.g., `"adding a training record"`).
#'   Should start with a verb in gerund form.
#' @param expectedMin Minimum number of rows expected to be affected. Default: 1.
#'   Most writes should affect at least one row; set to 0 only for operations
#'   where zero rows affected is a valid outcome (e.g., conditional deletes).
#' @param expectedMax Maximum number of rows expected to be affected. Default: 1.
#'   Increase for batch operations.
#' @param showMessage Logical. Whether to display a success modal on success.
#'   Default: `FALSE`. Set to `TRUE` only for the final write in a sequence;
#'   intermediate writes should stay silent.
#' @param notify Logical. Whether to call `NotifyError()` when the DB execute
#'   fails with an error (not just a row-count mismatch). Default: `TRUE`.
#'   Set to `FALSE` for writes inside loops where a single failure should not
#'   trigger a notification email.
#' @param namespace Character string used as the logger namespace. Default:
#'   `"DbExecuteAudited"`. Pass the calling function's name for easier
#'   log filtering (e.g., `namespace = "WriteIncident"`).
#'
#' @return Logical. `TRUE` if the write succeeded and rows affected is within
#'   `[expectedMin, expectedMax]`. `FALSE` otherwise.
#'
#' @details
#' **Execution order:**
#' 1. Calls `DBI::dbExecute()` inside a `tryCatch`. On error: logs, optionally
#'    calls `NotifyError()`, and returns `FALSE` immediately.
#' 2. Logs the SQL statement at `INFO` level.
#' 3. Calls `AuditLog()` to write to the `audit_log` table. Audit log failures
#'    are non-fatal (handled inside `AuditLog()`).
#' 4. Calls `CheckWriteResult()` to validate the row count and optionally
#'    display a success or error modal.
#'
#' **Note:** This function does not call `DBI::sqlInterpolate()`. Parameterize
#' your query before passing it in to retain full control over the connection
#' and parameter types.
#'
#' @section Transactional usage:
#' ```r
#' tryCatch({
#'   DBI::dbBegin(app_data$CON)
#'
#'   results <- c(
#'     DbExecuteAudited(sql1, current_user, context = "inserting incident"),
#'     DbExecuteAudited(sql2, current_user, context = "inserting incident unit")
#'   )
#'
#'   if (!all(results)) {
#'     DBI::dbRollback(app_data$CON)
#'     return(invisible(FALSE))
#'   }
#'
#'   DBI::dbCommit(app_data$CON)
#' }, error = function(e) {
#'   DBI::dbRollback(app_data$CON)
#'   NotifyError(e)
#' })
#' ```
#'
#' @examples
#' \dontrun{
#' # Single row insert — silent success, notify on failure
#' safe_sql <- DBI::sqlInterpolate(
#'   app_data$CON,
#'   "INSERT INTO training (firefighter_id, start_time) VALUES (?, ?)",
#'   ff_id, start_time
#' )
#' write_results <- c(
#'   write_results,
#'   DbExecuteAudited(safe_sql, current_user, context = "adding a training record")
#' )
#'
#' # Final write in a sequence — show success modal
#' write_results <- c(
#'   write_results,
#'   DbExecuteAudited(
#'     safe_sql,
#'     current_user,
#'     context     = "updating attendance",
#'     showMessage = TRUE
#'   )
#' )
#'
#' # Batch insert loop — suppress per-row notifications
#' for (ff_id in firefighter_ids) {
#'   write_results <- c(
#'     write_results,
#'     DbExecuteAudited(
#'       build_sql(ff_id),
#'       current_user,
#'       context     = "adding firefighter response",
#'       expectedMax = 1,
#'       notify      = FALSE,
#'       showMessage = FALSE
#'     )
#'   )
#' }
#'
#' # Conditional delete where 0 rows is valid
#' DbExecuteAudited(
#'   delete_sql,
#'   current_user,
#'   context     = "removing old units",
#'   expectedMin = 0,
#'   expectedMax = 100
#' )
#' }
#'
#' @seealso [AuditLog()], [CheckWriteResult()], [NotifyError()]
#' @export
DbExecuteAudited <- function(
  sql,
  currentUser,
  context = NULL,
  expectedMin = 1,
  expectedMax = 1,
  showMessage = FALSE,
  notify = TRUE,
  namespace = "DbExecuteAudited"
) {
  .CheckPackageEnv()
  app_data <- .pkg_env$app_data

  # --- Input validation ---
  sql_chr <- tryCatch(
    as.character(sql),
    error = function(e) NULL
  )

  # as.character(NULL) returns character(0), so check length first
  if (is.null(sql_chr) || length(sql_chr) == 0 || nchar(trimws(sql_chr)) == 0) {
    stop("'sql' must be a non-empty character string or DBI SQL object")
  }

  if (!is.numeric(expectedMin) || !is.numeric(expectedMax)) {
    stop("'expectedMin' and 'expectedMax' must be numeric")
  }

  if (expectedMin > expectedMax) {
    stop("'expectedMin' must be less than or equal to 'expectedMax'")
  }

  if (!is.logical(notify) || !is.logical(showMessage)) {
    stop("'notify' and 'showMessage' must be TRUE or FALSE")
  }

  # --- Execute ---
  result <- tryCatch(
    {
      DBI::dbExecute(app_data$CON, sql)
    },
    error = function(e) {
      context_text <- if (!is.null(context) && nzchar(context)) {
        context
      } else {
        "performing write"
      }
      logger::log_error(
        "Database execute failed when {context_text}: {e$message}",
        namespace = namespace
      )
      if (notify) {
        NotifyError(
          message = e$message,
          context = context %||% "",
          current_user = currentUser
        )
      }
      return(NA_integer_)
    }
  )

  # --- Log SQL ---
  logger::log_info(sql_chr, namespace = namespace)

  # --- Audit log (non-fatal if it fails; AuditLog handles that internally) ---
  AuditLog(sql_chr, currentUser)

  # --- Validate row count and return ---
  CheckWriteResult(
    result,
    context = context,
    expectedMin = expectedMin,
    expectedMax = expectedMax,
    showMessage = showMessage
  )
}
