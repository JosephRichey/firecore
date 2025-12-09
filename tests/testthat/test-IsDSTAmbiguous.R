test_that("IsDSTAmbiguous correctly identifies ambiguous times during fall back", {
  # Test ambiguous hour (1:00-1:59 AM) on DST fall back day
  expect_true(IsDSTAmbiguous("2024-11-03", "01:30", "America/Denver"))
  expect_true(IsDSTAmbiguous("2024-11-03", "01:00", "America/Denver"))
  expect_true(IsDSTAmbiguous("2024-11-03", "01:59", "America/Denver"))

  # Test with seconds in time format
  expect_true(IsDSTAmbiguous("2024-11-03", "01:00:00", "America/Denver"))
  expect_true(IsDSTAmbiguous("2024-11-03", "01:30:00", "America/Denver"))
})

test_that("IsDSTAmbiguous correctly identifies non-ambiguous times", {
  # Times outside the ambiguous hour on fall back day
  expect_false(IsDSTAmbiguous("2024-11-03", "00:30", "America/Denver"))
  expect_false(IsDSTAmbiguous("2024-11-03", "02:00", "America/Denver"))
  expect_false(IsDSTAmbiguous("2024-11-03", "03:00", "America/Denver"))
  expect_false(IsDSTAmbiguous("2024-11-03", "12:00", "America/Denver"))

  # Times on non-DST transition days
  expect_false(IsDSTAmbiguous("2024-11-04", "01:30", "America/Denver"))
  expect_false(IsDSTAmbiguous("2024-06-15", "01:30", "America/Denver"))

  # Spring forward day (not ambiguous, creates a gap instead)
  expect_false(IsDSTAmbiguous("2024-03-10", "02:30", "America/Denver"))
})

test_that("IsDSTAmbiguous works with different time zones", {
  # America/New_York (falls back on different date)
  expect_true(IsDSTAmbiguous("2024-11-03", "01:30", "America/New_York"))
  expect_false(IsDSTAmbiguous("2024-11-03", "02:30", "America/New_York"))

  # America/Los_Angeles
  expect_true(IsDSTAmbiguous("2024-11-03", "01:15", "America/Los_Angeles"))
  expect_false(IsDSTAmbiguous("2024-11-03", "03:30", "America/Los_Angeles"))

  # Europe/London (different fall back date)
  expect_true(IsDSTAmbiguous("2024-10-27", "01:30", "Europe/London"))

  # UTC has no DST
  expect_false(IsDSTAmbiguous("2024-11-03", "01:30", "UTC"))
})

test_that("IsDSTAmbiguous handles edge cases", {
  # Midnight (not ambiguous)
  expect_false(IsDSTAmbiguous("2024-11-03", "00:00", "America/Denver"))

  # End of ambiguous hour
  expect_true(IsDSTAmbiguous("2024-11-03", "01:59:59", "America/Denver"))

  # Just after ambiguous hour
  expect_false(IsDSTAmbiguous("2024-11-03", "02:00:00", "America/Denver"))
})
