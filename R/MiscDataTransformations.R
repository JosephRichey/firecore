# ============================================================================
# BuildNamedVector.R
# ============================================================================

#' Build a Named Vector from Data Frame Columns
#'
#' Creates a named vector from two columns of a data frame, optionally filtering
#' the data frame first. Commonly used for creating choice vectors for Shiny
#' select inputs where the display text differs from the underlying value.
#'
#' @param df A data frame containing the columns to use.
#' @param name Unquoted column name to use for the names of the vector.
#' @param value Unquoted column name to use for the values of the vector.
#' @param filterExpr Optional filter expression to subset the data frame before
#'   creating the vector. Use standard dplyr syntax (e.g., `status == "active"`).
#'
#' @return A named vector where names come from the `name` column and values
#'   come from the `value` column.
#'
#' @details
#' This function uses non-standard evaluation (NSE) via rlang's quosures,
#' allowing you to pass unquoted column names. The filter expression is also
#' evaluated using NSE.
#'
#' @examples
#' \dontrun{
#' # Create a simple lookup vector
#' firefighters <- data.frame(
#'   id = 1:3,
#'   name = c("John Doe", "Jane Smith", "Bob Johnson"),
#'   status = c("active", "active", "inactive")
#' )
#'
#' # All firefighters
#' BuildNamedVector(firefighters, name, id)
#' # Returns: c("John Doe" = 1, "Jane Smith" = 2, "Bob Johnson" = 3)
#'
#' # Only active firefighters
#' BuildNamedVector(firefighters, name, id, status == "active")
#' # Returns: c("John Doe" = 1, "Jane Smith" = 2)
#'
#' # Use in Shiny selectInput
#' selectInput("firefighter", "Choose:",
#'             choices = BuildNamedVector(firefighters, name, id))
#' }
#'
#' @export
BuildNamedVector <- function(df, name, value, filterExpr = NULL) {
  # Validate input
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }

  if (nrow(df) == 0) {
    warning("'df' is empty, returning empty named vector")
    return(stats::setNames(character(0), character(0)))
  }

  # Convert arguments to quosures
  nameQuosure <- rlang::enquo(name)
  valueQuosure <- rlang::enquo(value)
  filterQuosure <- rlang::enquo(filterExpr)

  # Apply filter if provided
  if (rlang::quo_is_null(filterQuosure)) {
    filteredDf <- df
  } else {
    filteredDf <- df |> dplyr::filter(!!filterQuosure)
  }

  # Check if filter resulted in empty data frame
  if (nrow(filteredDf) == 0) {
    warning("Filter expression resulted in empty data frame")
    return(stats::setNames(character(0), character(0)))
  }

  # Select and rename columns
  vectorDf <- filteredDf |>
    dplyr::select(name = !!nameQuosure, value = !!valueQuosure)

  # Create and return named vector
  return(stats::setNames(vectorDf$value, vectorDf$name))
}

# ============================================================================
# IdToString.R
# ============================================================================

#' Convert ID to String Value from Data Frame
#'
#' Looks up a string value in a data frame based on an ID. Commonly used for
#' converting database IDs to human-readable display names.
#'
#' @param df A data frame containing an 'id' column and the target column.
#' @param column Unquoted column name to extract the value from.
#' @param id The ID value to look up. Can be quoted or unquoted.
#'
#' @return A single value from the specified column, or a zero-length vector
#'   if no match is found.
#'
#' @details
#' The function expects the data frame to have a column named 'id'. If multiple
#' rows match the ID, only the first match is returned. If no rows match,
#' an empty vector is returned.
#'
#' @examples
#' \dontrun{
#' firefighters <- data.frame(
#'   id = 1:3,
#'   name = c("John Doe", "Jane Smith", "Bob Johnson"),
#'   rank = c("Captain", "Lieutenant", "Firefighter")
#' )
#'
#' # Get name by ID
#' IdToString(firefighters, name, 2)
#' # Returns: "Jane Smith"
#'
#' # Get rank by ID
#' IdToString(firefighters, rank, 1)
#' # Returns: "Captain"
#'
#' # Non-existent ID
#' IdToString(firefighters, name, 99)
#' # Returns: character(0)
#' }
#'
#' @export
IdToString <- function(df, column, id) {
  # Validate input
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }

  if (!"id" %in% colnames(df)) {
    stop("'df' must contain an 'id' column")
  }

  # Convert arguments to quosures
  columnQuosure <- rlang::enquo(column)
  idQuosure <- rlang::enquo(id)

  # Filter and extract value
  result <- df |>
    dplyr::filter(id == !!idQuosure) |>
    dplyr::select(!!columnQuosure) |>
    dplyr::pull()

  return(result)
}

# ============================================================================
# StringToId.R
# ============================================================================

#' Convert String Value to ID from Data Frame
#'
#' Looks up an ID in a data frame based on a string value in a specified column.
#' The inverse operation of `IdToString()`. Commonly used for converting
#' user-selected display names back to database IDs.
#'
#' @param df A data frame containing an 'id' column and the search column.
#' @param column Unquoted column name to search within.
#' @param value The value to search for in the specified column.
#'
#' @return The ID corresponding to the matched value, or a zero-length vector
#'   if no match is found.
#'
#' @details
#' The function expects the data frame to have a column named 'id'. If multiple
#' rows match the value, only the first match is returned. If no rows match,
#' an empty vector is returned.
#'
#' @examples
#' \dontrun{
#' firefighters <- data.frame(
#'   id = 1:3,
#'   name = c("John Doe", "Jane Smith", "Bob Johnson"),
#'   rank = c("Captain", "Lieutenant", "Firefighter")
#' )
#'
#' # Get ID by name
#' StringToId(firefighters, name, "Jane Smith")
#' # Returns: 2
#'
#' # Get ID by rank
#' StringToId(firefighters, rank, "Captain")
#' # Returns: 1
#'
#' # Non-existent value
#' StringToId(firefighters, name, "Unknown Person")
#' # Returns: integer(0)
#' }
#'
#' @export
StringToId <- function(df, column, value) {
  # Validate input
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame")
  }

  if (!"id" %in% colnames(df)) {
    stop("'df' must contain an 'id' column")
  }

  if (missing(value)) {
    stop("'value' must be provided")
  }

  # Convert column to quosure
  columnQuosure <- rlang::enquo(column)

  # Filter and extract ID
  result <- df |>
    dplyr::filter(!!columnQuosure == value) |>
    dplyr::select(id) |>
    dplyr::pull()

  return(result)
}

# ============================================================================
# FixColNames.R
# ============================================================================

#' Format Data Frame Column Names for Display
#'
#' Converts column names from snake_case to Title Case and optionally removes
#' a common prefix. Useful for preparing data frames for display in tables
#' where human-readable column names are preferred.
#'
#' @param data A data frame whose column names should be formatted.
#' @param prefix Optional character string prefix to remove from all column
#'   names after title casing. Common use: removing table name prefixes like
#'   "firefighter_" from column names.
#'
#' @return The input data frame with modified column names.
#'
#' @details
#' The function performs the following transformations in order:
#' 1. Replaces underscores with spaces
#' 2. Converts to Title Case (each word capitalized)
#' 3. Removes specified prefix if provided
#'
#' This modifies the data frame in place and returns it.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' df <- data.frame(
#'   first_name = "John",
#'   last_name = "Doe",
#'   hire_date = "2020-01-15"
#' )
#'
#' FixColNames(df)
#' # Column names: "First Name", "Last Name", "Hire Date"
#'
#' # With prefix removal
#' df2 <- data.frame(
#'   firefighter_id = 1,
#'   firefighter_name = "John",
#'   firefighter_rank = "Captain"
#' )
#'
#' FixColNames(df2, prefix = "Firefighter ")
#' # Column names: "Id", "Name", "Rank"
#'
#' # Use in Shiny table output
#' output$table <- renderTable({
#'   data |> FixColNames() |> head(10)
#' })
#' }
#'
#' @export
FixColNames <- function(data, prefix = NULL) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (!is.null(prefix) && (!is.character(prefix) || length(prefix) != 1)) {
    stop("'prefix' must be NULL or a single character string")
  }

  # Replace underscores with spaces
  colnames(data) <- gsub("_", " ", colnames(data))

  # Convert to Title Case
  colnames(data) <- stringr::str_to_title(colnames(data))

  # Remove prefix if provided
  if (!is.null(prefix)) {
    colnames(data) <- gsub(prefix, "", colnames(data))
  }

  return(data)
}
