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

