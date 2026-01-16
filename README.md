# firecore

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/JosephRichey/firecore)

## Overview

The goal of firecore is to make functions that are resuseud across the [FireData](https://github.com/JosephRichey/fire-data) suite of apps portable and resuable. It centralizes common operations like database access, timezone handling, audit logging, and data transformations to ensure consistency and reduce code duplication.

## Key Features

-   **🗄️ Database Operations**: Secure, schema-aware database queries with automatic validation
-   **🕐 Timezone Handling**: Robust datetime conversions with DST awareness for emergency response data
-   **📝 Audit Logging**: Comprehensive user action tracking for compliance
-   **⚛️ Reactive Data Management**: Bulk updates of reactive values with automatic transformations
-   **🔧 Utility Functions**: Common data manipulation tasks for Shiny apps

## Installation

``` r
# Install
remotes::install_github('JosephRichey/firecore')
```

## Quick Start

### 1. Configure Your App

In your app's `main.R`:

``` r
# Set default tables for this app
options(firecore.default_tables = c('training', 'firefighter', 'attendance'))
```

In your app's main server:

``` r
# Initiliaze package environment with app data.
InitializePackage(app_data)
```

## Core Functions

### Database Operations

#### `QueryDatabase(tableName)`

Query database tables with automatic schema support and SQL injection protection.

``` r
# Simple table query
patients <- QueryDatabase("firefighter")

# Schema-qualified query
patients <- QueryDatabase("dbo.firefighter")
```

#### `CheckWriteResult(result, successMessage, context, ...)`

Validate database write operations and display user feedback.

``` r
result <- dbExecute(con, "INSERT INTO training VALUES (...)")

CheckWriteResult(
  result, 
  successMessage = "Training record added",
  context = "inserting training",
  expectedMin = 1,
  expectedMax = 1
)
```

#### `AuditLog(userAction, session)`

Log user actions for compliance and security monitoring.

``` r
AuditLog("Viewed patient record #12345", session)
AuditLog("Modified training schedule", session)
```

### Reactive Data Management

#### `UpdateReactives(rdfs, dbTableName = NULL)`

Bulk update reactive data frames from database tables with automatic timezone conversions.

``` r
# Uses default tables from options
UpdateReactives(rdfs)

# Override with specific tables
UpdateReactives(rdfs, c("incidents", "apparatus"))
```

**Automatic Transformations:**

-   `training`: Converts `start_time` and `end_time` to local timezone

-   `attendance`: Converts `check_in` and `check_out` to local timezone

-   `incident`: Converts `incident_start` and `incident_end` to local timezone

-   `response`: Converts `response_start` and `response_end` to local timezone

-   `firefighter`: Converts `start_date` to local timezone and sorts by `display_order`

-   `firefighter_response`: Replaces NA `time_adjustment` values with 0

### Date/Time Utilities

#### `GenerateThreshold(date, leadTime, leadTimeUnit, expireCalc = FALSE)`

Calculate dates by adding or subtracting lead times.

``` r
# Calculate 30 days ago
GenerateThreshold(Sys.Date(), 30, "day")

# Calculate expiration date 6 months from now
GenerateThreshold(Sys.Date(), 6, "month", expireCalc = TRUE)
```

#### `ConvertToLocalPosix(x, input, output)`

Convert between UTC and local timezone with DST handling.

``` r
# UTC datetime to local datetime
ConvertToLocalPosix("2024-01-15 10:00:00", input = "datetime", output = "datetime")

# UTC date to local date
ConvertToLocalPosix("2024-01-15", input = "date", output = "date")
```

### Data Transformation

#### `BuildNamedVector(df, name, value, filterExpr = NULL)`

Create named vectors for Shiny select inputs.

``` r
# All firefighters
choices <- BuildNamedVector(firefighters, name, id)

# Only active firefighters
choices <- BuildNamedVector(firefighters, name, id, status == "active")

# Use in selectInput
selectInput("firefighter", "Choose:", choices = choices)
```

#### `IdToString(df, column, id)` / `StringToId(df, column, value)`

Convert between database IDs and display names.

``` r
# Get name from ID
name <- IdToString(firefighters, name, 42)

# Get ID from name
id <- StringToId(firefighters, name, "John Doe")
```

#### `FixColNames(data, prefix = NULL)`

Format column names for display tables.

``` r
# Convert snake_case to Title Case
data |> FixColNames()
# first_name -> "First Name"

# Remove prefix
data |> FixColNames(prefix = "Firefighter ")
# firefighter_id -> "Id"
```

## Configuration

### App-Level Configuration

Each app sets its default tables once in `global.R`:

``` r
# Training Management App
options(firecore.default_tables = c('training', 'firefighter', 'attendance'))

# Incident Management App
options(firecore.default_tables = c('incidents', 'apparatus', 'firefighter_response'))
```

### Database Connection

The package expects a global `app_data` object with a database connection:

``` r
app_data <- list(
  CON = DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "your-server",
    Database = "your-database"
  )
)
```

## Design Philosophy

### Timezone Awareness

All datetime functions in `firecore` are timezone-aware, critical for accurate reporting. The package handles:

-   UTC to local timezone conversions

-   DST transitions

-   Date-only vs datetime handling

### Security

-   **SQL Injection Protection**: All database functions use parameterized queries or proper identifier quoting
-   **Audit Logging**: Comprehensive tracking of user actions for compliance
-   **Input Validation**: All functions validate inputs before processing

### Shiny Integration

-   Functions work seamlessly with reactive values
-   Proper error handling with user-friendly modals
-   Logging for debugging and monitoring

### Portability

-   Package-level options eliminate need for app-specific wrappers
-   Functions are self-contained and well-documented
-   No hard-coded assumptions about data structure

## Testing

Run the test suite:

``` r
devtools::test()
```

The package includes comprehensive tests covering: - Database operations (mocked connections) - Timezone conversions (including DST edge cases) - Reactive context handling - Input validation - Error conditions

## Dependencies

### Required

-   DBI (\>= 1.1.0)
-   dplyr (\>= 1.0.0)
-   lubridate (\>= 1.9.0)
-   logger (\>= 0.2.0)
-   shinyalert (\>= 3.0.0)
-   stringr (\>= 1.5.0)
-   rlang (\>= 1.0.0)
-   glue (\>= 1.6.0)

### Suggested

-   shiny (for reactive context)
-   testthat (\>= 3.0.0, for testing)
-   withr (for test isolation)

## Common Patterns

### Full CRUD Operation with Audit Logging

``` r
observeEvent(input$save_button, {
  # Insert/Update data
  result <- dbExecute(
    app_data$CON,
    "INSERT INTO training (name, date) VALUES (?, ?)",
    params = list(input$name, input$date)
  )
  
  # Check result and show feedback
  if (CheckWriteResult(result, "Training saved", "saving training")) {
    # Log the action
    AuditLog(glue("Saved training: {input$name}"), session)
    
    # Refresh reactive data
    UpdateReactives(rdfs, "training")
  }
})
```

### Building Dynamic Filters

``` r
output$firefighter_select <- renderUI({
  req(rdfs$firefighter)
  
  # Build choices based on current filters
  choices <- BuildNamedVector(
    rdfs$firefighter,
    name,
    id,
    status == "active" & rank %in% input$rank_filter
  )
  
  selectInput("firefighter", "Select Firefighter:", choices = choices)
})
```

### Timezone-Safe Date Ranges

``` r
# Calculate lookback period
start_date <- GenerateThreshold(Sys.Date(), 30, "day")
end_date <- Sys.Date()

# Query with proper timezone handling
incidents <- QueryDatabase("incidents") |>
  filter(
    incident_start >= ConvertToLocalPosix(start_date, "date", "datetime"),
    incident_start <= ConvertToLocalPosix(end_date, "date", "datetime")
  )
```

## License

GPL (\>= 3)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

## Support

For questions or issues, contact: [jwrichey.1\@gmail.com](mailto:jwrichey.1@gmail.com)