#' Build a datetime from separate date and time components
#'
#' Combines a date and time into a single datetime object. Handles timezone
#' conversions and can process time inputs from Shiny time inputs (which
#' automatically include dates). Useful for reconstructing datetimes from
#' separate date and time picker inputs.
#'
#' @param time Time component as character string (e.g., "14:30:00", "14:30"),
#'   or as a POSIXct/POSIXlt object (time will be extracted automatically)
#' @param date Date component as Date object, character string, or any format
#'   that can be coerced to a date
#' @param input Timezone of the input date and time. One of:
#'   \itemize{
#'     \item \code{"local"} - Input is in local timezone (default)
#'     \item \code{"UTC"} - Input is in UTC timezone
#'   }
#' @param return_type Desired timezone of the returned datetime. One of:
#'   \itemize{
#'     \item \code{"UTC"} - Return datetime in UTC (default, useful for database storage)
#'     \item \code{"local"} - Return datetime in local timezone
#'   }
#'
#' @return A POSIXct datetime object in the requested timezone
#'
#' @details
#' The function retrieves the local timezone from application settings via
#' \code{\link{GetSetting}} (key: "ltz").
#'
#' When \code{time} is a POSIXct or POSIXlt object (as returned by Shiny time
#' inputs), the function automatically extracts just the time component using
#' \code{hms::as_hms()}, discarding any date information that may be present.
#'
#' The conversion process follows these steps:
#' \enumerate{
#'   \item Extract time component if input is POSIXct/POSIXlt
#'   \item Combine date and time into datetime string
#'   \item Parse as POSIXct in the input timezone
#'   \item Convert to the requested return timezone
#' }
#'
#' This function is particularly useful for Shiny applications where date and
#' time are collected in separate input widgets and need to be combined for
#' database storage (typically in UTC).
#'
#' @examples
#' \dontrun{
#' # Build datetime from character inputs (local timezone)
#' dt <- BuildDateTime(time = "14:30:00", date = "2025-01-15",
#'                     input = "local", return_type = "UTC")
#' # Returns: "2025-01-15 21:30:00 UTC" (if local is America/Edmonton MST)
#'
#' # Build from Shiny time input (which includes unwanted date)
#' shiny_time <- as.POSIXct("1970-01-01 14:30:00")
#' dt <- BuildDateTime(time = shiny_time, date = as.Date("2025-01-15"))
#' # Extracts time "14:30:00" and combines with correct date
#'
#' # Build datetime that stays in local timezone
#' dt_local <- BuildDateTime(time = "09:00:00", date = "2025-01-15",
#'                           input = "local", return_type = "local")
#'
#' # Build from UTC inputs
#' dt <- BuildDateTime(time = "21:30:00", date = "2025-01-15",
#'                     input = "UTC", return_type = "UTC")
#' }
#'
#' @seealso \code{\link{GetSetting}}, \code{\link{ConvertToLocalPosix}},
#'   \code{\link{FormatDateTime}}
#' @export
BuildDateTime <- function(
  time,
  date,
  input = c("local", "UTC"),
  return_type = c('UTC', 'local')
) {
  input <- match.arg(input)
  return_type <- match.arg(return_type)
  tz_local <- GetSetting("global", key = "ltz")

  # If input is posix, strip out time. shinyinputs automatically add a date.
  if (is.POSIXct(time) | is.POSIXlt(time)) {
    time <- time |>
      hms::as_hms() |>
      as.character()
  }

  # Store original requested time for validation
  requested_time <- time

  # Parse requested time to comparable format (handle HH:MM or HH:MM:SS)
  requested_time_parts <- strsplit(requested_time, ":")[[1]]
  if (length(requested_time_parts) == 2) {
    requested_time <- paste0(requested_time, ":00")
  }

  # Ensure consistent HH:MM:SS format
  requested_time <- format(strptime(requested_time, "%H:%M:%S"), "%H:%M:%S")

  # Check for ambiguous time during DST fall back
  input_tz <- if (input == 'local') tz_local else 'UTC'
  if (IsDSTAmbiguous(date, requested_time, input_tz)) {
    stop(glue::glue(
      "The datetime '{date} {requested_time}' is ambiguous in timezone '{input_tz}'. ",
      "This occurs during DST fall back when the same clock time occurs twice. ",
      "Please specify a different time or clarify which occurrence is intended."
    ))
  }

  dt <- as.POSIXct(
    paste(date, time),
    tz = if (input == 'local') tz_local else 'UTC'
  )

  # Check if the resulting time matches what was requested
  # This catches nonexistent times during DST spring forward
  actual_time <- format(dt, "%H:%M:%S")

  if (actual_time != requested_time) {
    stop(glue::glue(
      "The datetime '{date} {requested_time}' does not exist in timezone '{if (input == 'local') tz_local else 'UTC'}'. ",
      "This typically occurs during DST transitions (spring forward). ",
      "The requested time was automatically adjusted to {actual_time}."
    ))
  }

  # Return converted to the desired time zone
  if (return_type == 'local') {
    return(with_tz(dt, tzone = tz_local))
  } else {
    return(with_tz(dt, tzone = 'UTC'))
  }
}
