#' Send an Email via Microsoft Graph API
#'
#' General-purpose email sending function used by all FirePulse email features.
#' Sends from no-reply@myfirepulse.com via the Microsoft Graph API using
#' client credentials (app-only) authentication. All other email functions
#' in firecore (e.g. \code{\link{NotifyError}}) call this function internally.
#'
#' @param to Character vector of recipient email address(es).
#' @param subject Character string. Email subject line.
#' @param body Character string. Email body. Accepts plain text or HTML.
#'   Use \code{is_html = TRUE} when passing HTML content.
#' @param is_html Logical. If TRUE, body is sent as HTML. Default FALSE (plain text).
#' @param attachments Optional named list of attachments. Each element should be
#'   a list with fields: \code{name} (filename string) and \code{path} (absolute
#'   file path). Example:
#'   \code{list(list(name = "report.pdf", path = "/tmp/report.pdf"))}
#'   Attachment support is stubbed for future use -- currently accepted but not
#'   yet implemented in the Graph request body.
#'
#' @return Invisibly returns the httr2 response object on success, or NULL if
#'   the send was skipped (missing credentials) or failed. Failures are logged
#'   as warnings -- this function never throws, so email failures cannot mask
#'   application errors.
#'
#' @details
#' Credentials are read from environment variables. All four must be set for
#' the email to send:
#' \itemize{
#'   \item \code{GRAPH_TENANT_ID} -- Azure AD tenant ID
#'   \item \code{GRAPH_CLIENT_ID} -- App registration client ID
#'   \item \code{GRAPH_CLIENT_SECRET} -- App registration client secret
#'   \item \code{GRAPH_FROM_EMAIL} - The email address to send from. Must have
#'     Mail.Send permission granted in Azure AD.
#' }
#'
#' If any credential is missing the function logs a warning and returns NULL
#' without attempting a send. This allows the app to run in environments where
#' email is not configured (e.g., local development) without throwing errors.
#'
#' @seealso \code{\link{NotifyError}} for error notification emails.
#'
#' @examples
#' \dontrun{
#' # Plain text email
#' SendEmail(
#'   to      = "someone@example.com",
#'   subject = "Hello",
#'   body    = "This is a test message."
#' )
#'
#' # HTML email (e.g., automated report)
#' SendEmail(
#'   to      = c("chief@fd.gov", "admin@fd.gov"),
#'   subject = "Monthly Training Report",
#'   body    = "<h1>Report</h1><p>See summary below...</p>",
#'   is_html = TRUE
#' )
#'
#' # With attachment (future use)
#' SendEmail(
#'   to          = "admin@fd.gov",
#'   subject     = "Q1 Report",
#'   body        = "Please find the attached report.",
#'   attachments = list(list(name = "q1_report.pdf", path = "/tmp/q1_report.pdf"))
#' )
#' }
#'
#' @export
SendEmail <- function(
  to,
  subject,
  body,
  is_html = FALSE,
  attachments = NULL
) {
  # --- Validate inputs -------------------------------------------------------
  if (!is.character(to) || length(to) == 0) {
    stop("'to' must be a non-empty character vector of email addresses.")
  }

  if (!is.character(subject) || length(subject) != 1 || !nzchar(subject)) {
    stop("'subject' must be a single non-empty character string.")
  }

  if (!is.character(body) || length(body) != 1) {
    stop("'body' must be a single character string.")
  }

  # --- Read credentials from environment -------------------------------------
  tenant_id <- Sys.getenv("GRAPH_TENANT_ID")
  client_id <- Sys.getenv("GRAPH_CLIENT_ID")
  client_secret <- Sys.getenv("GRAPH_CLIENT_SECRET")
  from_email <- Sys.getenv("GRAPH_FROM_EMAIL")

  missing_vars <- c(
    if (!nzchar(tenant_id)) "GRAPH_TENANT_ID",
    if (!nzchar(client_id)) "GRAPH_CLIENT_ID",
    if (!nzchar(client_secret)) "GRAPH_CLIENT_SECRET",
    if (!nzchar(from_email)) "GRAPH_FROM_EMAIL"
  )

  if (length(missing_vars) > 0) {
    logger::log_warn(
      glue::glue(
        "SendEmail: skipping -- missing env vars: {paste(missing_vars, collapse = ', ')}"
      ),
      namespace = "SendEmail"
    )
    return(invisible(NULL))
  }

  # --- Warn if attachments passed (not yet implemented) ----------------------
  if (!is.null(attachments)) {
    logger::log_warn(
      "SendEmail: attachments parameter is not yet implemented. Email will be sent without attachments.",
      namespace = "SendEmail"
    )
  }

  # Wrap everything from here in tryCatch so email failures never throw
  tryCatch(
    {
      # --- Acquire access token ------------------------------------------------
      logger::log_trace(
        "SendEmail: acquiring Graph access token.",
        namespace = "SendEmail"
      )

      token_response <- httr2::request("https://login.microsoftonline.com") |>
        httr2::req_url_path_append(tenant_id) |>
        httr2::req_url_path_append("oauth2/v2.0/token") |>
        httr2::req_body_form(
          grant_type = "client_credentials",
          client_id = client_id,
          client_secret = client_secret,
          scope = "https://graph.microsoft.com/.default"
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      access_token <- token_response$access_token

      if (is.null(access_token) || !nzchar(access_token)) {
        stop("Graph token response did not contain an access_token.")
      }

      # --- Build recipient list ------------------------------------------------
      to_recipients <- lapply(to, function(addr) {
        list(emailAddress = list(address = addr))
      })

      # --- Send via Graph API --------------------------------------------------
      logger::log_trace(
        glue::glue("SendEmail: sending to {paste(to, collapse = ', ')}."),
        namespace = "SendEmail"
      )

      response <- httr2::request("https://graph.microsoft.com/v1.0") |>
        httr2::req_url_path_append("users", from_email, "sendMail") |>
        httr2::req_auth_bearer_token(access_token) |>
        httr2::req_body_json(list(
          message = list(
            subject = subject,
            body = list(
              contentType = if (is_html) "HTML" else "Text",
              content = body
            ),
            toRecipients = to_recipients
          ),
          saveToSentItems = TRUE
        )) |>
        httr2::req_perform()

      logger::log_info(
        glue::glue(
          "SendEmail: successfully sent to {paste(to, collapse = ', ')}."
        ),
        namespace = "SendEmail"
      )

      return(invisible(response))
    },
    error = function(e) {
      logger::log_warn(
        glue::glue("SendEmail: failed to send email -- {e$message}"),
        namespace = "SendEmail"
      )
      return(invisible(NULL))
    }
  )
}
