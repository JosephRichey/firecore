#' Parse Relative Date Strings
#'
#' `ParseRelativeDate()` converts a relative date string into an actual `Date`,
#' using a reference date (default: `app_data$current_local_date`).
#' It supports both **snap codes** (period-based anchors such as current month,
#' current quarter, current year) and **offset codes** (add/subtract days,
#' weeks, months, years).
#'
#' ## Supported Formats
#'
#' ### 1. Special Keywords
#' * `"today"` — returns the reference date unchanged.
#'
#' ### 2. Snap Codes (`CM`, `CQ`, `CY`)
#' Snap codes anchor to the reference date's **current** month/quarter/year.
#' They also support optional offsets:
#'
#' * `"CM"` — current month
#' * `"CQ"` — current quarter
#' * `"CY"` — current year
#'
#' You may append:
#' * `+N` (forward N units)
#' * `-N` (back N units)
#'
#' Examples:
#' * `"CM"` → current month
#' * `"CM+1"` → next month
#' * `"CQ-2"` → two quarters ago
#'
#' When `type = "start"` or `"end"`, the function returns the
#' start or end of that month/quarter/year (e.g., `"CM"` with
#' `type = "end"` returns the last day of the month).
#'
#' ### 3. Offset Codes
#' Offset codes adjust the reference date by a number of time units:
#'
#' * `"D+3"` — 3 days ahead
#' * `"W-1"` — 1 week ago
#' * `"M+2"` — 2 months ahead
#' * `"Y-5"` — 5 years ago
#'
#' ## Parameters
#' @param relativeString A character string describing the relative date.
#'   Supported formats include `"today"`, snap codes (`"CM"`, `"CQ+1"`,
#'   `"CY-2"`), and offset codes (`"D+3"`, `"M-1"`, `"W+2"`, `"Y-1"`).
#' @param type Whether to return the `"start"` or `"end"` of the period for
#'   snap codes. Ignored for simple offset formats. Defaults to `"start"`.
#' @param refDate The reference `Date` from which calculations are made.
#'   Defaults to `app_data$current_local_date`.
#'
#' ## Returns
#' A `Date` object representing the resolved date.
#'
#' ## Errors
#' Throws an error if `relativeString` does not match any supported format.
#'
#' @examples
#' # Must call initialize package before use
#' InitializePackage(list(current_local_date = Sys.time() |> lubridate::with_tz('America/Denver') |> as.Date()))
#' # Using snap codes
#' ParseRelativeDate("CM")           # Start of current month
#' ParseRelativeDate("CM", "end")    # End of current month
#' ParseRelativeDate("CQ+1")         # Start of next quarter
#'
#' # Using offsets
#' ParseRelativeDate("D+3")          # Three days from refDate
#' ParseRelativeDate("M-1")          # One month before refDate
#'
#' @export
#' @importFrom lubridate "%m+%" floor_date ceiling_date days weeks years with_tz


ParseRelativeDate <- function(relativeString, type = c("start", "end"), refDate = .pkg_env$app_data$current_local_date) {
  .CheckPackageEnv()

  type <- match.arg(type)
  today <- refDate

  relativeString <- toupper(relativeString)

  # Handle 'today' explicitly
  if (relativeString == "TODAY") {
    return(refDate)
  }

  # Match snap patterns: CM, CQ, CY, optionally with +N/-N
  snap_match <- stringr::str_match(relativeString, "^(C[MQY])([+-]?)(\\d*)$")
  # snap_match has 4 columns: full match, snap unit, offset direction, amount

  # First, check there is a match for the snap pattern
  if (!is.na(snap_match[1, 1])) {
    snap_unit <- snap_match[1, 2]  # CM, CQ, CY
    sign <- snap_match[1, 3]
    amount <- snap_match[1, 4]

    # If amount is empty, default to 0
    offset <- if (amount == "") 0 else as.integer(amount)
    # and invert if sign is '-'
    if (sign == "-") offset <- -offset

    base_date <- switch(
      snap_unit,
      "CM" = refDate %m+% months(offset),
      "CQ" = refDate %m+% months(3 * offset),
      "CY" = refDate %m+% years(offset),
      stop("Unknown snap code")
    )

    return(switch(
      snap_unit,
      "CM" = if (type == "start") lubridate::floor_date(base_date, "month") else lubridate::ceiling_date(base_date, "month") - lubridate::days(1),
      "CQ" = if (type == "start") lubridate::floor_date(base_date, "quarter") else lubridate::ceiling_date(base_date, "quarter") - lubridate::days(1),
      "CY" = if (type == "start") lubridate::floor_date(base_date, "year") else lubridate::ceiling_date(base_date, "year") - lubridate::days(1)
    ))
  }

  # Offset-only logic: M-1, D+3, etc.
  match <- stringr::str_match(relativeString, "^([MWDY])([+-])(\\d+)$")

  if (any(is.na(match))) {
    stop(glue::glue("Invalid relative date format: '{relativeString}'"))
  }

  unit <- match[, 2]
  sign <- match[, 3]
  n <- as.integer(match[, 4])

  offset <- if (sign == "-") -n else n

  result <- switch(
    unit,
    "M" = refDate %m+% months(offset),
    "W" = refDate + lubridate::weeks(offset),
    "D" = refDate + lubridate::days(offset),
    "Y" = refDate %m+% years(offset),
    stop("Unknown unit")
  )

  return(result)
}
