#' Format date and time objects
#'
#' Converts datetime or date objects to formatted strings in either local timezone
#' or UTC. The formatting follows settings defined in the application configuration
#' for dates and datetimes, or uses standard time formats.
#'
#' @param dt A datetime (POSIXct) or date (Date) object to format
#' @param input Type of input object. One of:
#'   \itemize{
#'     \item \code{"datetime"} - POSIXct datetime object (default)
#'     \item \code{"date"} - Date object
#'   }
#' @param output Desired output format. One of:
#'   \itemize{
#'     \item \code{"datetime"} - Full date and time (default)
#'     \item \code{"date"} - Date only
#'     \item \code{"time"} - Time only
#'   }
#' @param target_tz Target timezone for the output. One of:
#'   \itemize{
#'     \item \code{"local"} - Local timezone from settings (default)
#'     \item \code{"UTC"} - UTC timezone
#'   }
#' @param seconds Logical. Whether to include seconds in time output.
#'   Default is \code{FALSE}. Only applies when \code{output = "time"}.
#'
#' @return A character string with the formatted date/time
#'
#' @details
#' The function uses application settings to determine date and datetime formats
#' via \code{\link{GetSetting}}. UTC datetime outputs always use the standard
#' format "%Y-%m-%d %H:%M:%S" for database compatibility.
#'
#' Invalid input-output combinations will raise an error:
#' \itemize{
#'   \item Cannot format date input as time output
#'   \item Cannot format date input as datetime output
#'   \item Cannot format time input as datetime output
#' }
#'
#' @examples
#' \dontrun{
#' dt <- as.POSIXct("2025-01-15 14:30:00")
#'
#' # Format as local datetime
#' FormatDateTime(dt, input = "datetime", output = "datetime", target_tz = "local")
#'
#' # Format as date only
#' FormatDateTime(dt, input = "datetime", output = "date")
#'
#' # Format as time with seconds
#' FormatDateTime(dt, input = "datetime", output = "time", seconds = TRUE)
#'
#' # Format for database (UTC)
#' FormatDateTime(dt, output = "datetime", target_tz = "UTC")
#' }
#'
#' @seealso \code{\link{GetSetting}}
#' @export
FormatDateTime <- function(
    dt,
    input = c("datetime", "date"),
    output = c("datetime", "date", "time"),
    target_tz = c("local", "UTC"),
    seconds = FALSE) {

  input <- match.arg(input)
  output <- match.arg(output)
  target_tz <- match.arg(target_tz)

  # Restrict incompatible input-output combinations
  invalid_combo <- (input == "date"   && output == "time")     ||
    (input == "time"   && output == "datetime") ||
    (input == "date"   && output == "datetime")

  if (invalid_combo) {
    stop(glue::glue("Invalid conversion: cannot format input type '{input}' as output type '{output}'"))
  }

  tz <- switch(
    target_tz,
    "local" = GetSetting('global', key = 'ltz'),
    "UTC" = "UTC"
  )

  # dt should already by POSIXct or date, but just to be safe
  # Normalize input to POSIXct based on type
  dt <- switch(
    input,
    "date" = as.Date(dt),
    "datetime" = as.POSIXct(dt, tz = tz)
  )

  # Format as string based on desired output
  fmt <- switch(
    output,
    "date" = GetSetting('global', key = 'date_format'),
    "time" = if (seconds) "%H:%M:%S" else "%H:%M",
    "datetime" = GetSetting('global', key = 'date_time_format')
  )

  # UTC outputs go to the database and must be in this format
  if(output == 'datetime' & target_tz == 'UTC') {
    fmt <- "%Y-%m-%d %H:%M:%S"
  }

  format(dt, fmt, tz = tz, usetz = FALSE)
}


