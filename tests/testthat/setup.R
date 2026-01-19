# # Mock ConvertToLocalPosix globally
# ConvertToLocalPosix <- function(x, input, output) {
#   return(x)
# }
# assignInNamespace("ConvertToLocalPosix", ConvertToLocalPosix, ns = "firecore")

# # Create a default test database connection
# default_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# # Initialize package environment with real connection
# app_data <- list(CON = default_con)
# InitializePackage(app_data)
# .CheckPackageEnv()
