#' Query All Records from a Database Table
#'
#' Retrieves all records from a specified database table with error handling
#' and logging. This is a convenience wrapper around `dbGetQuery()` for simple
#' full-table queries.
#'
#' @param tableName A character string specifying the name of the database
#'   table to query. The table name will be validated to prevent SQL injection.
#'
#' @return A data frame containing all records from the specified table, or
#'   NULL if the query fails. Returns an empty data frame if the table exists
#'   but contains no records.
#'
#' @details
#' The function performs a simple `SELECT * FROM table_name` query. For more
#' complex queries with WHERE clauses, JOINs, or specific column selection,
#' use `DBI::dbGetQuery()` directly with parameterized queries.
#'
#' Error and warning messages are logged using the `logger` package with the
#' "QueryDatabase" namespace for easier debugging and monitoring.
#'
#' @section Security:
#' The function validates the table name against SQL naming conventions to
#' prevent SQL injection attacks. Only alphanumeric characters, underscores,
#' and periods are allowed in table names.
#'
#' @examples
#' \dontrun{
#' # Query all records from a table
#' patients <- QueryDatabase("patients")
#'
#' # Query with schema qualification
#' patients <- QueryDatabase("dbo.patients")
#'
#' # Check if query was successful
#' if (!is.null(patients)) {
#'   print(nrow(patients))
#' }
#'
#' # Query a table that might not exist
#' data <- QueryDatabase("optional_table")
#' if (is.null(data)) {
#'   message("Table does not exist or query failed")
#' }
#' }
#'
#' @export
QueryDatabase <- function(tableName) {
  .CheckPackageEnv()

  app_data <- .pkg_env$app_data

  # Validate input
  if (!is.character(tableName) || length(tableName) != 1) {
    stop("'tableName' must be a single character string")
  }

  if (nchar(tableName) == 0) {
    stop("'tableName' cannot be empty")
  }

  # Validate table name to prevent SQL injection
  # Only allow alphanumeric, underscore, and period
  if (!grepl("^[a-zA-Z0-9_.]+$", tableName)) {
    stop(
      "'tableName' contains invalid characters. Only letters, numbers, underscores, and periods are allowed"
    )
  }

  # Check if connection exists
  if (is.null(app_data$CON)) {
    logger::log_error(
      "Database connection not found",
      namespace = "QueryDatabase"
    )
    stop("Database connection (app_data$CON) is not available")
  }

  # Handle schema.table notation
  # Split on period to identify schema and table separately
  if (grepl("\\.", tableName)) {
    parts <- strsplit(tableName, "\\.")[[1]]
    if (length(parts) == 2) {
      # Quote schema and table separately, then combine
      quotedSchema <- DBI::dbQuoteIdentifier(app_data$CON, parts[1])
      quotedTable <- DBI::dbQuoteIdentifier(app_data$CON, parts[2])
      quotedIdentifier <- paste0(quotedSchema, ".", quotedTable)
    } else {
      stop("'tableName' must be in format 'table' or 'schema.table'")
    }
  } else {
    # Single table name, quote it
    quotedIdentifier <- DBI::dbQuoteIdentifier(app_data$CON, tableName)
  }

  # Build query with properly quoted identifier
  query <- paste0("SELECT * FROM ", quotedIdentifier)

  # Execute query with error handling
  data <- tryCatch(
    {
      DBI::dbGetQuery(app_data$CON, query)
    },
    error = function(e) {
      logger::log_error(
        "Database query failed for table '{tableName}': {e$message}",
        namespace = "QueryDatabase"
      )
      return(NULL)
    },
    warning = function(w) {
      logger::log_warn(
        "Database query warning for table '{tableName}': {w$message}",
        namespace = "QueryDatabase"
      )
    }
  )

  return(data)
}
