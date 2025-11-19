#' Get Setting
#'
#' @param domain The general area the setting is used (training, incident, etc.)
#' @param key The specific key that identifies the setting.
#' @param group Some settings are grouped in groups. You can get all these settings
#' by specifying a group.
#'
#' @returns The desired setting in a character vector.
#' @export
#'
#' @examples
#' GetSetting()
GetSetting <- function(domain, key = NULL, group = NULL) {
  # Filtered <- app_data$Setting |>
  #   filter(domain == !!domain)
  #
  # if (!is.null(group)) {
  #   Filtered <- Filtered |> filter(setting_group == !!group)
  # }
  #
  # if (!is.null(key)) {
  #   Filtered <- Filtered |> filter(setting_key == !!key)
  # }
  #
  # # If no matching key, return nothing and show a warning
  # if (nrow(Filtered) == 0) {
  #   log_error(glue("No setting found for domain '{domain}' and key '{key}'"),
  #             namespace = "GetSetting")
  #   return(NA)
  # }
  #
  # result <- Filtered$setting_value
  # type <- Filtered$value_type[1] # Assuming same type for all
  #
  # coerce <- switch(
  #   type,
  #   "string" = as.character,
  #   "numeric" = as.numeric,
  #   "boolean" = function(x) tolower(x) %in% c("true", "1", "t", "yes"),
  #   "date" = function(x) as.Date(x),
  #   "relative_date" = function(x) ParseRelativeDate(x),
  #   as.character # fallback
  # )
  #
  # result <- coerce(result)
  # return(
  #   if (length(result) == 1) result[[1]] else result
  # )

  return('Hello from GetSetting')
}
