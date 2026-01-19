# tests/testthat/teardown.R

# Clean up default connection
if (!is.null(firecore:::.pkg_env$app_data$CON)) {
  DBI::dbDisconnect(firecore:::.pkg_env$app_data$CON)
}

cat('Removed test db connection\n', file = stderr())
