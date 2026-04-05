#' Send an Error Notification Email
#'
#' Logs an error and sends a notification email to the FirePulse error inbox.
#' Called automatically by \code{\link{DbExecuteAudited}} on database failures,
#' and can be called directly from any \code{error = function(e)} handler in
#' application code.
#'
#' This function never throws. If the email send fails (e.g., missing credentials,
#' network issue), the failure is logged as a warning and execution continues.
#' The original error is always logged regardless of whether the email succeeds.
#'
#' @param message Character string. The error message to include in the
#'   notification. Typically \code{e$message} from a tryCatch error handler.
#' @param context Character string. Describes where the error occurred, e.g.
#'   \code{"Delete Training"} or \code{"UpdateAttendance"}. Used in both the
#'   log entry and the email subject. Default: empty string.
#' @param current_user A reactiveVal, reactive, reactiveValues, or plain
#'   character string identifying the user who triggered the error. Accepts
#'   the same types as \code{\link{AuditLog}}. Default: NULL (logged as
#'   "Unknown").
#'
#' @return Invisibly returns NULL. Called for its side effects (logging +
#'   email) only.
#'
#' @details
#' The error email is always sent to \code{error@myfirepulse.com}, which is
#' read from the \code{ERROR_EMAIL} environment variable. The email includes:
#' \itemize{
#'   \item The context (where the error occurred)
#'   \item The error message
#'   \item The username (if available)
#'   \item A timestamp
#' }
#'
#' The \code{ERROR_EMAIL} environment variable must be set for emails to send.
#' If it is missing, a warning is logged and no email is sent -- the function
#' still logs the error at the \code{log_error} level regardless.
#' Additionally, \code{FIRE_DEPARTMENT} and \code{APPLICATION} need to be set to
#' have the email sent with complete details.
#'
#' @seealso \code{\link{SendEmail}} for general-purpose email sending.
#' @seealso \code{\link{DbExecuteAudited}} which calls this on database errors.
#'
#' @examples
#' \dontrun{
#' # In a tryCatch error handler
#' tryCatch(
#'   {
#'     dbExecute(conn, sql)
#'   },
#'   error = function(e) {
#'     NotifyError(e$message, context = "Delete Training", current_user = current_user)
#'     shinyalert(title = "Error", text = "Something went wrong.", type = "error")
#'   }
#' )
#'
#' # In a poolWithTransaction block
#' tryCatch(
#'   pool::poolWithTransaction(CON, function(conn) {
#'     # ... db writes ...
#'   }),
#'   error = function(e) {
#'     NotifyError(e$message, context = "Submit Incident", current_user = current_user)
#'   }
#' )
#' }
#'
#' @export
NotifyError <- function(message, context = "", current_user = NULL) {
  # --- Resolve username (mirrors AuditLog logic) ----------------------------
  username <- tryCatch(
    {
      user <- if (shiny::is.reactivevalues(current_user)) {
        current_user$name
      } else if (shiny::is.reactive(current_user)) {
        current_user()
      } else {
        current_user
      }

      if (is.null(user) || !nzchar(user)) "Unknown" else as.character(user)
    },
    error = function(e) "Unknown"
  )

  # --- Always log the error first -------------------------------------------
  # Log regardless of whether the email succeeds.
  context_label <- if (nzchar(context)) glue::glue("[{context}] ") else ""

  logger::log_error(
    glue::glue("{context_label}Error affecting user '{username}': {message}"),
    namespace = "NotifyError"
  )

  # --- Check ERROR_EMAIL env var --------------------------------------------
  error_email <- Sys.getenv("ERROR_EMAIL")

  if (!nzchar(error_email)) {
    logger::log_warn(
      "NotifyError: ERROR_EMAIL env var not set -- skipping notification email.",
      namespace = "NotifyError"
    )
    return(invisible(NULL))
  }

  # --- Build email content --------------------------------------------------
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  subject <- if (nzchar(context)) {
    glue::glue("[FirePulse Error] {context}")
  } else {
    "[FirePulse Error] Application Error"
  }

  # HTML body -- readable in email clients and logged clearly
  body <- glue::glue(
    '
    <h2 style="color:#c0392b;">FirePulse Application Error</h2>
    <table style="border-collapse:collapse;font-family:sans-serif;font-size:14px;">
        <tr>
        <td style="padding:4px 12px 4px 0;font-weight:bold;">Fire Department</td>
        <td>{Sys.getenv("FIRE_DEPARTMENT")}</td>
      </tr>
      <tr>
        <td style="padding:4px 12px 4px 0;font-weight:bold;">App</td>
        <td>{Sys.getenv("APPLICATION")}</td>
      </tr>
      <tr>
        <td style="padding:4px 12px 4px 0;font-weight:bold;">Context</td>
        <td>{if (nzchar(context)) context else "Not specified"}</td>
      </tr>
      <tr>
        <td style="padding:4px 12px 4px 0;font-weight:bold;">User</td>
        <td>{username}</td>
      </tr>
      <tr>
        <td style="padding:4px 12px 4px 0;font-weight:bold;">Time</td>
        <td>{timestamp}</td>
      </tr>
      <tr>
        <td style="padding:4px 12px 4px 0;font-weight:bold;vertical-align:top;">Error</td>
        <td style="font-family:monospace;background:#f8f8f8;padding:4px;">{message}</td>
      </tr>
    </table>
    <p style="color:#888;font-size:12px;margin-top:24px;">
      Sent automatically by FirePulse. Do not reply to this email.
    </p>
  '
  )

  # --- Delegate to SendEmail ------------------------------------------------
  # SendEmail handles its own tryCatch, so this cannot throw.
  SendEmail(
    to = error_email,
    subject = subject,
    body = body,
    is_html = TRUE
  )

  return(invisible(NULL))
}
