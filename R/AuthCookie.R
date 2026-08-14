#' Create signed-authentication cookie settings
#' @param secret Private signing secret.
#' @param cookie_name Browser cookie name.
#' @param lifetime Cookie lifetime in seconds.
#' @export
NewAuthCookieConfig <- function(secret, cookie_name = "firecore_auth", lifetime = 28800) {
  if (!is.character(secret) || length(secret) != 1 || !nzchar(secret)) stop("A cookie secret is required.", call. = FALSE)
  list(secret = secret, cookie_name = cookie_name, lifetime = lifetime)
}

.AuthCookieSignature <- function(payload, config) paste(format(openssl::sha256(payload, key = config$secret)), collapse = "")

#' Create a signed authentication cookie token
#' @param user_id A single, stable user identifier to store in the token.
#' @param config Settings returned by [NewAuthCookieConfig()].
#' @param now Current time, injectable for testing.
#' @export
CreateAuthCookie <- function(user_id, config, now = Sys.time()) {
  expires_at <- as.integer(now) + config$lifetime
  payload <- paste("v1", user_id, expires_at, sep = ".")
  paste(payload, .AuthCookieSignature(payload, config), sep = ".")
}

#' Validate a signed authentication cookie token
#' @param cookie Token read from the browser cookie.
#' @param config Settings returned by [NewAuthCookieConfig()].
#' @param now Current time, injectable for testing.
#' @return The user ID, or `NULL` for an invalid or expired token.
#' @export
ValidateAuthCookie <- function(cookie, config, now = Sys.time()) {
  if (!is.character(cookie) || length(cookie) != 1 || !nzchar(cookie)) return(NULL)
  parts <- strsplit(cookie, ".", fixed = TRUE)[[1]]
  if (length(parts) != 4 || parts[[1]] != "v1") return(NULL)
  payload <- paste(parts[1:3], collapse = ".")
  expires_at <- suppressWarnings(as.numeric(parts[[3]]))
  if (is.na(expires_at) || expires_at <= as.numeric(now) || !identical(parts[[4]], .AuthCookieSignature(payload, config))) return(NULL)
  parts[[2]]
}

#' Load browser support for signed authentication cookies
#' @export
AuthCookieUI <- function() htmltools::attachDependencies(shiny::tagList(), htmltools::htmlDependency(name = "firecore-auth-cookie", version = "1.0.0", src = system.file("www", package = "firecore"), script = "auth-cookie.js"))

#' Write or refresh a signed browser cookie
#' @param session The active Shiny session.
#' @param firefighter_id The stable firefighter ID to store in the token.
#' @param config Settings returned by [NewAuthCookieConfig()].
#' @export
WriteFirefighterCookie <- function(session, firefighter_id, config) {
  session$sendCustomMessage("firecore-write-auth-cookie", list(cookie_name = config$cookie_name, value = CreateAuthCookie(firefighter_id, config), max_age = config$lifetime))
}
