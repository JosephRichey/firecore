#' Check Database Write Result and Display User Feedback
#'
#' Validates the result of a database write operation (INSERT, UPDATE, DELETE)
#' and displays appropriate success or error modals to the user. Includes
#' logging for monitoring and debugging.
#'
#' @param result Numeric value returned by `DBI::dbExecute()` indicating the
#'   number of rows affected by the operation.
#' @param successMessage Character string displayed in the success modal.
#'   Default: "Write successful."
#' @param context Optional character string providing context for the operation
#'   (e.g., "inserting training record", "updating attendance"). Used in error
#'   messages and logs. Should start with a verb in gerund form.
#' @param expectedMin Minimum number of rows expected to be affected. Default: 0
#' @param expectedMax Maximum number of rows expected to be affected. Default: 1
#' @param showMessage Logical indicating whether to display a success modal.
#'   Set to FALSE for silent success (still logs). Default: TRUE
#'
#' @return Logical. Returns TRUE if the write operation was successful (result
#'   is within expected range), FALSE otherwise.
#'
#' @details
#' The function validates that:
#' \itemize{
#'   \item `result` is a numeric value
#'   \item `result` is not NA
#'   \item `result` is within the greater than or equal to expectedMin and less than or equal to expectedMax
#' }
#'
#' **Success behavior:**
#' - Optionally displays a success modal (if `showMessage = TRUE`)
#' - Logs success message
#' - Returns TRUE
#'
#' **Failure behavior:**
#' - Displays error modal with context and result details
#' - Logs error message
#' - Returns FALSE
#'
#' @section Usage Patterns:
#' **Single row insert:**
#' ```r
#' result <- dbExecute(con, "INSERT INTO ...")
#' CheckWriteResult(result, "Training record added", "inserting training")
#' ```
#'
#' **Update with expected range:**
#' ```r
#' result <- dbExecute(con, "UPDATE firefighter SET ...")
#' CheckWriteResult(result, "Firefighters updated", "updating firefighters",
#'                  expectedMin = 1, expectedMax = 10)
#' ```
#'
#' **Silent success for batch operations:**
#' ```r
#' result <- dbExecute(con, "DELETE FROM ...")
#' CheckWriteResult(result, context = "deleting old records", showMessage = FALSE)
#' ```
#'
#' @examples
#' \dontrun{
#' # Basic usage - single row insert
#' result <- dbExecute(con, "INSERT INTO training VALUES (...)")
#' if (CheckWriteResult(result, "Training added successfully", "inserting training")) {
#'   # Continue with next operation
#' }
#'
#' # Allow multiple rows to be affected
#' result <- dbExecute(con, "UPDATE attendance SET ... WHERE department = 'A'")
#' CheckWriteResult(result, "Attendance updated", "updating attendance",
#'                  expectedMin = 1, expectedMax = 50)
#'
#' # Silent success (no modal)
#' result <- dbExecute(con, "DELETE FROM temp_table")
#' if (CheckWriteResult(result, context = "deleting temp data", showMessage = FALSE)) {
#'   log_info("Temp data cleaned up")
#' }
#'
#' # Delete operation that might affect 0 rows (valid)
#' result <- dbExecute(con, "DELETE FROM logs WHERE date < ?", old_date)
#' CheckWriteResult(result, "Old logs removed", "deleting old logs",
#'                  expectedMin = 0, expectedMax = 1000)
#' }
#'
#' @export
CheckWriteResult <- function(
  result,
  successMessage = "Write successful.",
  context = NULL,
  expectedMin = 0,
  expectedMax = 1,
  showMessage = TRUE
) {
  # Validate inputs
  if (missing(result)) {
    stop("'result' must be provided")
  }

  if (!is.character(successMessage) || length(successMessage) != 1) {
    stop("'successMessage' must be a single character string")
  }

  if (!is.null(context) && (!is.character(context) || length(context) != 1)) {
    stop("'context' must be NULL or a single character string")
  }

  if (!is.numeric(expectedMin) || !is.numeric(expectedMax)) {
    stop("'expectedMin' and 'expectedMax' must be numeric")
  }

  if (expectedMin > expectedMax) {
    stop("'expectedMin' must be less than or equal to 'expectedMax'")
  }

  if (!is.logical(showMessage)) {
    stop("'showMessage' must be TRUE or FALSE")
  }

  # Format context for messages
  contextText <- if (!is.null(context)) paste0("when ", context) else ""

  # Check if write was successful
  if (
    !is.numeric(result) ||
      is.na(result) ||
      result < expectedMin ||
      result > expectedMax
  ) {
    # Log failure
    logger::log_error(
      "Database write failed {contextText}. Result: {result}, Expected: {expectedMin}-{expectedMax}",
      namespace = "CheckWriteResult"
    )

    # Show error modal
    shinyalert::shinyalert(
      title = "Error",
      text = glue::glue(
        "Database write failed {contextText}. ",
        "Result: {result}. ",
        "Please contact your application administrator."
      ),
      type = "error",
      closeOnClickOutside = TRUE
    )

    return(FALSE)
  }

  # Success path
  if (showMessage) {
    shinyalert::shinyalert(
      title = "Success",
      text = successMessage,
      type = "success",
      closeOnClickOutside = TRUE
    )
  }

  # Log success
  logger::log_success(
    "Database write successful {contextText}. Rows affected: {result}",
    namespace = "CheckWriteResult"
  )

  return(TRUE)
}
