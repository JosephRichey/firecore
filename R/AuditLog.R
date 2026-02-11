#' Log User Actions to Audit Trail
#'
#' Records user actions to the audit_log database table with timestamp and
#' username information. Used for compliance, security monitoring, and
#' activity tracking. Username comes from the currentUser reactive passed
#' from the IdentifyFirefighter module.
#'
#' @param userAction A character string describing the action performed by
#'   the user (e.g., "Viewed patient record", "Modified data", "Logged in").
#' @param currentUser A reactiveVal or reactiveValues object containing the
#'   current user's name. If NULL or empty, logs as "Unknown".
#'
#' @return Invisibly returns the number of rows affected (typically 1 on success).
#'
#' @details
#' The function logs three pieces of information:
#' \itemize{
#'   \item Username: Extracted from `currentUser()`, or "Unknown" if NULL
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
#' # Create reactive user
#' currentUser <- reactiveVal("John Doe")
#'
#' # Log a patient record view
#' AuditLog("Viewed patient record #12345", currentUser)
#'
#' # Log a data modification
#' AuditLog("Updated patient demographics", currentUser)
#'
#' # Log a login event
#' AuditLog("User logged in", currentUser)
#' }
#'
#' @export
AuditLog <- function(userAction, currentUser) {
  .CheckPackageEnv()
  app_data <- .pkg_env$app_data

  # Validate inputs
  if (!is.character(userAction) || length(userAction) != 1) {
    cat("\n!!! VALIDATION FAILED !!!\n")
    cat("Stopping with error\n\n")
    stop("'userAction' must be a single character string")
  }

  # Extract username safely from reactive
  username <- tryCatch(
    {
      # Handle both reactiveVal() and reactiveValues()$field
      user <- if (shiny::is.reactivevalues(currentUser)) {
        currentUser$name # Adjust field name as needed
      } else if (shiny::is.reactive(currentUser)) {
        currentUser()
      } else {
        currentUser # In case it's passed as a plain value
      }

      if (is.null(user) || user == "") {
        "Unknown"
      } else {
        user
      }
    },
    error = function(e) {
      warning("Could not access currentUser: ", e$message)
      "Unknown"
    }
  )

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
