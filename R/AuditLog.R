#' Log User Actions to Audit Trail
#'
#' Records user actions to the audit_log database table with timestamp and
#' username information. Used for compliance, security monitoring, and
#' activity tracking. Username comes when using the IdentifyFirefighter module.
#'
#' @param userAction A character string describing the action performed by
#'   the user (e.g., "Viewed patient record", "Modified data", "Logged in").
#'
#' @return Invisibly returns the number of rows affected (typically 1 on success).
#'
#' @details
#' The function logs three pieces of information:
#' \itemize{
#'   \item Username: Extracted from `session$user`, or "Unknown" if NULL
#'   \item Timestamp: Current system time in "YYYY-MM-DD HH:MM:SS" format
#'   \item User action: The description provided in `userAction`
#' }
#'
#' The function uses parameterized queries via `sqlInterpolate()` to prevent
#' SQL injection attacks. The database connection is accessed via
#' `app_data$CON`.
#'
#' @examples
#' \dontrun{
#' # Log a patient record view
#' AuditLog("Viewed patient record #12345")
#'
#' # Log a data modification
#' AuditLog("Updated patient demographics")
#'
#' # Log a login event
#' AuditLog("User logged in")
#' }
#'
#' @export
AuditLog <- function(userAction) {
  .CheckPackageEnv()

  app_data <- .pkg_env$app_data

  # Validate inputs
  if (!is.character(userAction) || length(userAction) != 1) {
    stop("'userAction' must be a single character string")
  }

  # Extract username safely
  username <- if (is.null(app_data$Current_User)) {
    "Unknown"
  } else {
    app_data$Current_User
  }

  # Format current timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Prepare SQL statement with parameterized query
  sql <- "INSERT INTO audit_log (username, date_time, user_action) VALUES (?, ?, ?)"

  # Safely interpolate values to prevent SQL injection
  safeSql <- DBI::sqlInterpolate(
    app_data$CON,
    sql,
    username,
    timestamp,
    userAction
  )

  # Execute the command and capture result
  result <- tryCatch(
    {
      DBI::dbExecute(app_data$CON, safeSql)
    },
    error = function(e) {
      warning("Failed to write to audit log: ", e$message)
      return(0)
    }
  )

  return(invisible(result))
}
