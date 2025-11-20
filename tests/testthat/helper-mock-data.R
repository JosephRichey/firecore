# tests/testthat/helper-mock-data.R

cat('Intitializing package for tests\n', file = stderr())
.GetMockAppData <- function() {
  list(
    Setting = data.frame(
      id = 1:23,
      domain = c(
        # Global settings (1-7)
        "global", "global", "global", "global", "global", "global", "global",
        # Incident settings (8-13)
        "incident", "incident", "incident", "incident", "incident", "incident",
        # Test settings for numeric (14)
        "test",
        # Test settings for date (15)
        "test",
        # Test settings for boolean variations (16-21)
        "test", "test", "test", "test", "test", "test",
        # Test settings for unknown type (22)
        "test",
        # Test settings for relative date (23)
        "test"
      ),
      setting_group = c(
        # Global
        "id", "time", "time", "time", "format", "id", "config",
        # Incident
        "incident_response", "incident_response", "incident_response",
        "incident_response", "incident_response", "incident_response",
        # Test
        "numbers", "dates", "bools", "bools", "bools", "bools", "bools", "bools",
        "misc", "dates"
      ),
      setting_key = c(
        # Global
        "password", "tz", "date_format", "date_time_format", "sidebar_open",
        "fire_dept_id", "safe_delete",
        # Incident
        "input_secondary", "canceled", "dropped", "address",
        "incident_cad_regex", "cad_id",
        # Test
        "max_count", "start_date", "bool1", "bool2", "bool3", "bool4",
        "bool5", "bool6", "unknown_type", "relative"
      ),
      setting_value = c(
        # Global
        "123", "America/Denver", "%m-%d-%Y", "%m-%d-%Y %H:%M", "TRUE",
        "Crabaple", "FALSE",
        # Incident
        "FALSE", "TRUE", "TRUE", "TRUE", "(?:\\d[7])", "Must be exactly 9",
        # Test
        "42", "2025-01-15", "true", "TRUE", "1", "yes", "false", "0",
        "some_value", "today"
      ),
      value_type = c(
        # Global
        "string", "string", "string", "string", "boolean", "string", "boolean",
        # Incident
        "boolean", "boolean", "boolean", "boolean", "string", "string",
        # Test
        "numeric", "date", "boolean", "boolean", "boolean", "boolean",
        "boolean", "boolean", "unknown", "relative_date"
      ),
      description = c(
        # Global
        "This password", "Time zone", "Date format", "Date time format",
        "Will the sidebar", "Title, display name", "Will there be",
        # Incident
        "Are secondary", "Is the canceled", "Is the dropdown", "Is the address",
        "Regex for category", "Message to show",
        # Test
        "Test numeric", "Test date", "Test bool", "Test bool", "Test bool",
        "Test bool", "Test bool", "Test bool", "Test unknown", "Test relative date"
      ),
      stringsAsFactors = FALSE
    ),
    current_local_date = Sys.time() |> with_tz('America/Denver') |> as.Date()
  )
}

# Initialize once for all tests
InitializePackage(.GetMockAppData())
