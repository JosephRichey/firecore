test_that("authentication cookies validate only while unaltered and current", {
  config <- NewAuthCookieConfig("test-secret", lifetime = 60)
  now <- as.POSIXct("2026-08-14 12:00:00", tz = "UTC")
  token <- CreateAuthCookie("42", config, now)
  expect_equal(ValidateAuthCookie(token, config, now), "42")
  expect_null(ValidateAuthCookie(token, config, now + 61))
  expect_null(ValidateAuthCookie(paste0(token, "x"), config, now))
})
