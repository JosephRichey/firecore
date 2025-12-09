#' Check if a Date and Time is Ambiguous Due to DST Fall Back
#'
#' Determines whether a given date and time falls within an ambiguous hour
#' during Daylight Saving Time fall back transition. In North America, when
#' clocks "fall back" (typically at 2:00 AM to 1:00 AM), the hour from
#' 1:00-1:59 AM occurs twice, making it ambiguous.
#'
#' @param date A character string or Date object representing the date to check,
#'   in a format parseable by `as.Date()` (e.g., "2024-11-03").
#' @param time A character string representing the time in "HH:MM" or "HH:MM:SS"
#'   format (e.g., "01:30" or "01:30:00").
#' @param tz A character string specifying the time zone to check for DST
#'   transitions (e.g., "America/Denver").
#'
#' @return A logical value: `TRUE` if the date and time fall within an ambiguous
#'   hour during DST fall back, `FALSE` otherwise.
#'
#' @examples
#' # Check if 1:30 AM on November 3, 2024 is ambiguous in Denver
#' IsDSTAmbiguous("2024-11-03", "01:30", "America/Denver")
#'
#' # Non-ambiguous time on the same day
#' IsDSTAmbiguous("2024-11-03", "03:00", "America/Denver")
#'
#' # Time on a non-DST transition day
#' IsDSTAmbiguous("2024-11-04", "01:30", "America/Denver")
#'
#' @export
IsDSTAmbiguous <- function(date, time, tz) {
  # Parse the time
  time_parts <- strsplit(time, ":")[[1]]
  if (length(time_parts) == 2) {
    time <- paste0(time, ":00")
  }

  # For America/Denveer (and most North American zones):
  # Fall back happens at 2:00 AM -> 1:00 AM
  # So 1:00-1:59 AM is ambiguous on fall back day

  date_obj <- as.Date(date)

  # Parse hour from time
  hour <- as.integer(time_parts[1])
  minute <- as.integer(time_parts[2])

  # Check if this is a DST fall back date in this timezone
  # Create times at midnight and 3 AM on this date
  dt_midnight <- as.POSIXct(paste(date, "00:00:00"), tz = tz)
  dt_3am <- as.POSIXct(paste(date, "03:00:00"), tz = tz)

  # If there's a DST transition, the difference will be more than 3 hours
  time_diff <- as.numeric(difftime(dt_3am, dt_midnight, units = "hours"))

  is_fall_back_day <- time_diff > 3

  # In North America, 1:00-1:59 AM is the ambiguous hour
  is_ambiguous_hour <- (hour == 1)

  return(is_fall_back_day && is_ambiguous_hour)
}
