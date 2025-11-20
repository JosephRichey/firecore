# Package-level environment for storing shared data
.pkg_env <- new.env(parent = emptyenv())

#' Initialize the firecore package
#'
#' This function must be called once at application startup to initialize the
#' firecore package with your application data. It stores the app_data object
#' in the package environment, making it available to all firecore functions
#' like \code{\link{GetSetting}}.
#'
#' @param app_data A list containing your application's data tables. This is what
#' is loaded into the app_data module.
#'
#' @return Invisibly returns \code{TRUE} on success. Prints a confirmation message.
#'
#' @examples
#' \dontrun{
#' # Load your application data
#' app_data <- list(
#'   current_local_date = Sys.time() |> lubridate::with_tz('America/Denver') |> as.Date()
#' )
#'
#' # Initialize the package
#' InitializePackage(app_data)
#'
#' # Now you can use other firecore functions
#' ParseRelativeDate('CM')
#' }
#'
#' @seealso \code{\link{GetSetting}}
#' @export
InitializePackage <- function(app_data) {
  .pkg_env$app_data <- app_data
  invisible(TRUE)
  cat('firecore successfully initialized\n', file = stderr())
}
