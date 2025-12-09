#' Convert UTC datetime to local timezone
#'
#' Converts a datetime or date object from UTC to the local timezone specified
#' in application settings. This is the inverse operation of converting local
#' times to UTC for database storage.
#'
#' @param dt A datetime (POSIXct) in UTC or a date (Date) object to convert
#' @param input Type of input object. One of:
#'   \itemize{
#'     \item \code{"datetime"} - POSIXct datetime object in UTC (default)
#'     \item \code{"date"} - Date object
#'   }
#' @param output Desired output format. One of:
#'   \itemize{
#'     \item \code{"datetime"} - POSIXct datetime in local timezone (default)
#'     \item \code{"date"} - Date object in local timezone
#'   }
#'
#' @return A POSIXct datetime in local timezone or a Date object, depending on
#'   the \code{output} parameter
#'
#' @details
#' The function retrieves the local timezone from application settings via
#' \code{\link{GetSetting}} (key: "ltz"). All datetime inputs are assumed to be
#' in UTC and are converted to the local timezone.
#'
#' When converting datetime to date (\code{output = "date"}), the date is
#' calculated in the local timezone. This means a UTC datetime of
#' "2025-01-15 23:00:00 UTC" might become "2025-01-16" in a timezone ahead of UTC.
#'
#' Invalid input-output combinations will raise an error:
#' \itemize{
#'   \item Cannot convert date input to datetime output (dates have no time component)
#' }
#'
#' @examples
#' \dontrun{
#' # Convert UTC datetime to local datetime
#' dt_utc <- as.POSIXct("2025-01-15 21:30:00", tz = "UTC")
#' dt_local <- ConvertToLocalPosix(dt_utc, input = "datetime", output = "datetime")
#' # Returns: "2025-01-15 14:30:00 MST" (if local TZ is America/Edmonton)
#'
#' # Convert UTC datetime to local date
#' dt_local_date <- ConvertToLocalPosix(dt_utc, input = "datetime", output = "date")
#' # Returns: Date object for "2025-01-15" in local timezone
#'
#' # Convert date to date (pass-through)
#' date_obj <- as.Date("2025-01-15")
#' result <- ConvertToLocalPosix(date_obj, input = "date", output = "date")
#' # Returns: Same date object
#' }
#'
#' @seealso \code{\link{GetSetting}}, \code{\link{FormatDateTime}}
#' @export
ConvertToLocalPosix <- function(dt,
                                input = c("datetime", "date"),
                                output = c("datetime", "date")) {
  input <- match.arg(input)
  output <- match.arg(output)
  # Restrict incompatible input-output combinations
  invalid_combo <- (input == "date"   && output == "datetime")
  if (invalid_combo) {
    stop(glue::glue("Invalid conversion: cannot format input type '{input}' as output type '{output}'"))
  }
  tz_local <- GetSetting("global", "ltz")
  # Dates can only be converted to dates, so return as is
  if (input == "date" && output == "date") {
    return(as.Date(dt))
  }
  # Normalize input into UTC
  dt_utc <- as.POSIXct(dt, tz = "UTC")
  # Convert to local timezone
  dt_local <- lubridate::with_tz(dt_utc, tzone = tz_local)
  if(input == 'datetime' && output == 'date') {
    return(as.Date(dt_local, tz = tz_local))
  }
  # Default is to return as datetime.
  return(dt_local)
}
