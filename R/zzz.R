# R/zzz.R

# Package-level environment for storing shared data
.pkg_env <- new.env(parent = emptyenv())

#' Initialize the package data store
#'
#' @param app_data The app_data object containing Setting and other tables
#' @export
InitializePackage <- function(app_data) {
  print('Intitializing package')
  .pkg_env$app_data <- app_data
  invisible(TRUE)
}
