# Private helper to check initialization
.CheckPackageEnv <- function() {
  if (is.null(.pkg_env$app_data)) {
    stop("Package not initialized. Call InitializePackage(app_data) first.")
  }
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "firecore package loaded. ",
    "Call InitializePackage(app_data) before using package functions."
  )
}

# utils-global-variables.R

# Suppress R CMD check notes about global variables
# These are either:
# - NSE (non-standard evaluation) column names used in dplyr/tidyverse operations
# - Global variables expected to exist in the calling environment (app_data)

utils::globalVariables(c(
  # Global environment variables
  "app_data",

  # NSE column names used in dplyr operations
  "id",
  "setting_group",
  "setting_key",
  "start_time",
  "end_time",
  "check_in",
  "check_out",
  "response_start",
  "response_end",
  "incident_start",
  "incident_end",
  "start_date",
  "display_order",
  "time_adjustment",
  "full_name",
  "is_active",
  "id",
  "firefighter_pin",
  "officer",
  "is_deleted"
))

# Note: is.POSIXct and is.POSIXlt should be prefixed with lubridate::
# Update BuildDateTime to use lubridate::is.POSIXct() and lubridate::is.POSIXlt()
