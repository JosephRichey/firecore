#' Calculate Threshold Date Based on Lead Time
#'
#' Calculates a date by adding or subtracting a lead time from a given date.
#' Useful for determining deadlines, expiration dates, or lookback periods.
#'
#' @param date A Date object or vector of Date objects representing the
#'   reference date(s).
#' @param leadTime A numeric value or vector indicating the number of time
#'   units to add or subtract.
#' @param leadTimeUnit A character string specifying the time unit. Must be
#'   one of "day", "month", or "year".
#' @param expireCalc Logical. If TRUE, adds lead time to the date (for
#'   expiration calculations). If FALSE (default), subtracts lead time from
#'   the date (for lookback calculations).
#'
#' @return A Date object or vector of Date objects representing the calculated
#'   threshold date(s).
#'
#' @details
#' The function uses month arithmetic that preserves end-of-month dates when
#' possible. For example, adding 1 month to January 31st results in February 28th
#' (or 29th in leap years).
#'
#' @examples
#' # Calculate a date 30 days before today
#' GenerateThreshold(Sys.Date(), 30, "day")
#'
#' # Calculate expiration date 6 months from now
#' GenerateThreshold(Sys.Date(), 6, "month", expireCalc = TRUE)
#'
#' # Calculate a date 2 years ago
#' GenerateThreshold(as.Date("2024-01-15"), 2, "year")
#'
#' @export
GenerateThreshold <- function(
  date,
  leadTime,
  leadTimeUnit,
  expireCalc = FALSE
) {
  # Validate inputs
  if (!inherits(date, "Date")) {
    stop("'date' must be a Date object")
  }

  if (!is.numeric(leadTime) || any(leadTime < 0)) {
    stop("'leadTime' must be a non-negative numeric value")
  }

  validUnits <- c("day", "month", "year")
  if (!leadTimeUnit %in% validUnits) {
    stop("'leadTimeUnit' must be one of: ", paste(validUnits, collapse = ", "))
  }

  if (!is.logical(expireCalc)) {
    stop("'expireCalc' must be TRUE or FALSE")
  }

  # Calculate threshold date
  if (expireCalc) {
    # Add lead time for expiration calculations
    result <- dplyr::case_when(
      leadTimeUnit == "day" ~ date + lubridate::days(leadTime),
      leadTimeUnit == "month" ~ date |> lubridate::`%m+%`(months(leadTime)),
      leadTimeUnit == "year" ~ date |>
        lubridate::`%m+%`(lubridate::years(leadTime))
    )
  } else {
    # Subtract lead time for lookback calculations
    result <- dplyr::case_when(
      leadTimeUnit == "day" ~ date - lubridate::days(leadTime),
      leadTimeUnit == "month" ~ date |> lubridate::`%m-%`(months(leadTime)),
      leadTimeUnit == "year" ~ date |>
        lubridate::`%m-%`(lubridate::years(leadTime))
    )
  }

  return(result)
}
