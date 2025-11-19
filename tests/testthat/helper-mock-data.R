# tests/testthat/helper-mock-data.R

get_mock_app_data <- function() {
  list(
    Setting = data.frame(
      id = 1:13,
      domain = c("global", "global", "global", "global", "global", "global",
                 "global", "incident", "incident", "incident", "incident",
                 "incident", "incident"),
      setting_group = c("id", "time", "time", "time", "format", "id",
                        "config", "incident_response", "incident_response",
                        "incident_response", "incident_response",
                        "incident_response", "incident_response"),
      setting_key = c("password", "tz", "date_format", "date_time_format",
                      "sidebar_open", "fire_dept_id", "safe_delete",
                      "input_secondary", "canvas", "dropped", "address",
                      "incident_category_regex", "cadastral_id"),
      setting_value = c("123", "America/Edmonton", "%m-%d-%Y", "%m-%d-%Y %H:%M",
                        "TRUE", "Crabaple", "FALSE", "FALSE", "TRUE", "TRUE",
                        "TRUE", "(?:\\d[7])", "Must be exactly 9"),
      value_type = c("string", "string", "string", "string", "boolean",
                     "string", "boolean", "boolean", "boolean", "boolean",
                     "boolean", "string", "string"),
      description = c("This password", "Time zone", "Date format",
                      "Date time format", "Will the sidebar",
                      "Title, display name", "Will there be",
                      "Are secondary", "Is the canvas", "Is the dropdown",
                      "Is the address", "Regex for category",
                      "Message to show"),
      stringsAsFactors = FALSE
    )
  )
}

# Initialize once for all tests
InitializePackage(get_mock_app_data())
print('Intitializing package for tests')
