#' Update Reactive Data Frames from Database Tables
#'
#' Queries specified database tables and applies appropriate transformations,
#' primarily timezone conversions for datetime fields. Results are stored in
#' a reactive values object for use in Shiny applications.
#'
#' @param rdfs A reactive values object (typically created with `reactiveValues()`)
#'   where the queried and transformed data frames will be stored.
#' @param dbTableName A character vector of table names to query and update.
#'   If NULL (default), uses the value from `getOption("firecore.default_tables")`.
#'   If neither is specified, an error is raised.
#'
#' @return Invisibly returns NULL. The function modifies `rdfs` as a side effect,
#'   storing each table's data frame with the table name as the key.
#'
#' @details
#' The function applies table-specific transformations based on the table name:
#' \itemize{
#'   \item **training**: Converts start_time and end_time to local timezone
#'   \item **attendance**: Converts check_in and check_out to local timezone
#'   \item **response**: Converts response_start and response_end to local timezone
#'   \item **incident**: Converts incident_start and incident_end to local timezone
#'   \item **firefighter**: Converts start_date to local timezone and sorts by display_order
#'   \item **firefighter_response**: Sets NA time_adjustment values to 0
#'   \item Other tables: No transformation applied
#' }
#'
#' All datetime conversions use the `ConvertToLocalPosix()` function from this
#' package to ensure consistent timezone handling across the application.
#'
#' @section Configuration:
#' Set default tables for your app in global.R using:
#' \preformatted{
#' options(firecore.default_tables = c('training', 'firefighter', 'attendance'))
#' }
#'
#' Once configured, you can call `UpdateReactives(rdfs)` without specifying tables.
#' You can always override the default by explicitly passing `dbTableName`.
#'
#' @examples
#' \dontrun{
#' # In your app's global.R - configure default tables once
#' options(firecore.default_tables = c('training', 'firefighter', 'attendance'))
#'
#' # In a Shiny server function
#' server <- function(input, output, session) {
#'   # Create reactive values object
#'   rdfs <- reactiveValues()
#'
#'   # Update using configured defaults
#'   UpdateReactives(rdfs)
#'
#'   # Or override the defaults for specific needs
#'   UpdateReactives(rdfs, c("incidents", "apparatus"))
#'
#'   # Access the data
#'   output$table <- renderTable({
#'     rdfs$training
#'   })
#' }
#'
#' # Different apps can have different configurations
#' # Training App global.R:
#' options(firecore.default_tables = c('training', 'attendance', 'firefighter'))
#'
#' # Incident App global.R:
#' options(firecore.default_tables = c('incidents', 'apparatus', 'firefighter_response'))
#' }
#'
#' @export
UpdateReactives <- function(rdfs, dbTableName = NULL) {
  # Validate rdfs input
  if (missing(rdfs) || !inherits(rdfs, "reactivevalues")) {
    stop("'rdfs' must be a reactivevalues object")
  }

  # Get table names from parameter or option
  if (is.null(dbTableName)) {
    dbTableName <- getOption("firecore.default_tables")

    if (is.null(dbTableName)) {
      stop(
        "'dbTableName' must be specified or set via options(firecore.default_tables = c(...)). ",
        "Add this to your app's global.R file."
      )
    }

    logger::log_debug(
      "Using default tables from options: {paste(dbTableName, collapse = ', ')}",
      namespace = "UpdateReactives"
    )
  }

  # Validate table names
  if (!is.character(dbTableName) || length(dbTableName) == 0) {
    stop("'dbTableName' must be a non-empty character vector")
  }

  logger::log_info(
    "Updating {length(dbTableName)} rdfs table(s): {paste(dbTableName, collapse = ', ')}",
    namespace = "UpdateReactives"
  )

  # Process each table
  for (name in dbTableName) {
    # Query the database
    df <- QueryDatabase(name)

    # Skip if query failed
    if (is.null(df)) {
      logger::log_warn(
        "Skipping table '{name}' - query returned NULL",
        namespace = "UpdateReactives"
      )
      next
    }

    # Apply table-specific transformations
    df <- switch(
      name,

      # Training table - convert start_time and end_time to local time
      "training" = df |>
        dplyr::mutate(
          start_time = ConvertToLocalPosix(
            start_time,
            input = 'datetime',
            output = 'datetime'
          ),
          end_time = ConvertToLocalPosix(
            end_time,
            input = 'datetime',
            output = 'datetime'
          )
        ),

      # Attendance table - convert check_in and check_out to local time
      "attendance" = df |>
        dplyr::mutate(
          check_in = ConvertToLocalPosix(
            check_in,
            input = 'datetime',
            output = 'datetime'
          ),
          check_out = ConvertToLocalPosix(
            check_out,
            input = 'datetime',
            output = 'datetime'
          )
        ),

      # Response table - convert response_start and response_end to local time
      "response" = df |>
        dplyr::mutate(
          response_start = ConvertToLocalPosix(
            response_start,
            input = 'datetime',
            output = 'datetime'
          ),
          response_end = ConvertToLocalPosix(
            response_end,
            input = 'datetime',
            output = 'datetime'
          )
        ),

      # Incident table - convert incident_start and incident_end to local time
      "incident" = df |>
        dplyr::mutate(
          incident_start = ConvertToLocalPosix(
            incident_start,
            input = 'datetime',
            output = 'datetime'
          ),
          incident_end = ConvertToLocalPosix(
            incident_end,
            input = 'datetime',
            output = 'datetime'
          )
        ),

      # Firefighter table - convert start_date and sort by display order
      "firefighter" = df |>
        dplyr::mutate(
          start_date = ConvertToLocalPosix(
            start_date,
            input = 'date',
            output = 'date'
          )
        ) |>
        dplyr::arrange(display_order),

      # Firefighter response - handle NA time adjustments
      "firefighter_response" = df |>
        dplyr::mutate(
          time_adjustment = dplyr::if_else(
            is.na(time_adjustment),
            0,
            time_adjustment
          )
        ),

      # Default case - no transformation needed
      df
    )

    # Store in reactive values
    rdfs[[name]] <- df

    logger::log_trace(
      "Table '{name}' updated: {nrow(df)} rows, {ncol(df)} columns",
      namespace = "UpdateReactives"
    )
  }

  logger::log_success(
    "All rdfs tables updated successfully. Tables: {paste(dbTableName, collapse = ', ')}",
    namespace = "UpdateReactives"
  )

  return(invisible(NULL))
}
