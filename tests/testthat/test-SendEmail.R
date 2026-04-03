# Tests for SendEmail() and NotifyError()
# Run with: testthat::test_file("tests/testthat/test-email.R")

library(testthat)

# Helper to set all four Graph credentials at once
set_graph_envvars <- function() {
  Sys.setenv(
    GRAPH_TENANT_ID = "fake-tenant-id",
    GRAPH_CLIENT_ID = "fake-client-id",
    GRAPH_CLIENT_SECRET = "fake-client-secret",
    GRAPH_FROM_EMAIL = "no-reply@myfirepulse.com"
  )
}

# Helper to clear all Graph credentials and ERROR_EMAIL
clear_email_envvars <- function() {
  Sys.unsetenv(c(
    "GRAPH_TENANT_ID",
    "GRAPH_CLIENT_ID",
    "GRAPH_CLIENT_SECRET",
    "GRAPH_FROM_EMAIL",
    "ERROR_EMAIL"
  ))
}

# Always clean up after the whole file runs
withr::defer(clear_email_envvars(), teardown_env())

# ==============================================================================
# SendEmail — input validation
# These throw before touching env vars or the network, no mocking needed.
# ==============================================================================

test_that("SendEmail throws when 'to' is not a character", {
  expect_error(
    SendEmail(to = 123, subject = "Test", body = "Test body"),
    "'to' must be a non-empty character vector"
  )
})

test_that("SendEmail throws when 'to' is empty character vector", {
  expect_error(
    SendEmail(to = character(0), subject = "Test", body = "Test body"),
    "'to' must be a non-empty character vector"
  )
})

test_that("SendEmail throws when 'subject' is empty string", {
  expect_error(
    SendEmail(to = "a@b.com", subject = "", body = "Test body"),
    "'subject' must be a single non-empty character string"
  )
})

test_that("SendEmail throws when 'subject' is not character", {
  expect_error(
    SendEmail(to = "a@b.com", subject = 99, body = "Test body"),
    "'subject' must be a single non-empty character string"
  )
})

test_that("SendEmail throws when 'body' is not a single character string", {
  expect_error(
    SendEmail(to = "a@b.com", subject = "Test", body = c("line1", "line2")),
    "'body' must be a single character string"
  )
})

# ==============================================================================
# SendEmail — credential bail-out
# Returns NULL without throwing when any env var is missing.
# ==============================================================================

test_that("SendEmail returns NULL when all credentials are missing", {
  clear_email_envvars()
  expect_null(SendEmail(to = "a@b.com", subject = "Test", body = "Body"))
})

test_that("SendEmail does not throw when all credentials are missing", {
  clear_email_envvars()
  expect_no_error(SendEmail(to = "a@b.com", subject = "Test", body = "Body"))
})

test_that("SendEmail returns NULL when only GRAPH_TENANT_ID is missing", {
  set_graph_envvars()
  Sys.unsetenv("GRAPH_TENANT_ID")
  expect_null(SendEmail(to = "a@b.com", subject = "Test", body = "Body"))
})

test_that("SendEmail returns NULL when only GRAPH_CLIENT_ID is missing", {
  set_graph_envvars()
  Sys.unsetenv("GRAPH_CLIENT_ID")
  expect_null(SendEmail(to = "a@b.com", subject = "Test", body = "Body"))
})

test_that("SendEmail returns NULL when only GRAPH_CLIENT_SECRET is missing", {
  set_graph_envvars()
  Sys.unsetenv("GRAPH_CLIENT_SECRET")
  expect_null(SendEmail(to = "a@b.com", subject = "Test", body = "Body"))
})

test_that("SendEmail returns NULL when only GRAPH_FROM_EMAIL is missing", {
  set_graph_envvars()
  Sys.unsetenv("GRAPH_FROM_EMAIL")
  expect_null(SendEmail(to = "a@b.com", subject = "Test", body = "Body"))
})

# ==============================================================================
# SendEmail — attachment handling
# ==============================================================================

test_that("SendEmail returns NULL cleanly when attachments passed with no credentials", {
  clear_email_envvars()
  expect_no_error({
    result <- SendEmail(
      to = "a@b.com",
      subject = "Test",
      body = "Body",
      attachments = list(list(name = "file.pdf", path = "/tmp/file.pdf"))
    )
    expect_null(result)
  })
})

# ==============================================================================
# NotifyError — never throws, always returns NULL
# ==============================================================================

test_that("NotifyError does not throw when ERROR_EMAIL is not set", {
  clear_email_envvars()
  expect_no_error(NotifyError("Something broke"))
})

test_that("NotifyError does not throw with context provided", {
  clear_email_envvars()
  expect_no_error(NotifyError("Something broke", context = "Delete Training"))
})

test_that("NotifyError does not throw with a plain character user", {
  clear_email_envvars()
  expect_no_error(
    NotifyError("Something broke", context = "ctx", current_user = "Jane Doe")
  )
})

test_that("NotifyError does not throw when current_user is NULL", {
  clear_email_envvars()
  expect_no_error(
    NotifyError("Something broke", context = "ctx", current_user = NULL)
  )
})

test_that("NotifyError returns NULL invisibly", {
  clear_email_envvars()
  expect_null(NotifyError("error message"))
})

# ==============================================================================
# NotifyError — subject line construction
# Mock SendEmail to inspect what NotifyError passes to it.
# ==============================================================================

test_that("NotifyError subject includes context when provided", {
  set_graph_envvars()
  Sys.setenv(ERROR_EMAIL = "error@myfirepulse.com")

  captured_subject <- NULL

  with_mocked_bindings(
    SendEmail = function(
      to,
      subject,
      body,
      is_html = FALSE,
      attachments = NULL
    ) {
      captured_subject <<- subject
      invisible(NULL)
    },
    {
      NotifyError("Something broke", context = "Delete Training")
      expect_match(captured_subject, "Delete Training")
      expect_match(captured_subject, "\\[FirePulse Error\\]")
    }
  )
})

test_that("NotifyError subject falls back to generic when context is empty", {
  set_graph_envvars()
  Sys.setenv(ERROR_EMAIL = "error@myfirepulse.com")

  captured_subject <- NULL

  with_mocked_bindings(
    SendEmail = function(
      to,
      subject,
      body,
      is_html = FALSE,
      attachments = NULL
    ) {
      captured_subject <<- subject
      invisible(NULL)
    },
    {
      NotifyError("Something broke", context = "")
      expect_match(captured_subject, "Application Error")
      expect_match(captured_subject, "\\[FirePulse Error\\]")
    }
  )
})

# ==============================================================================
# NotifyError — delegates to SendEmail correctly
# ==============================================================================

test_that("NotifyError sends to ERROR_EMAIL address", {
  set_graph_envvars()
  Sys.setenv(ERROR_EMAIL = "error@myfirepulse.com")

  captured_to <- NULL

  with_mocked_bindings(
    SendEmail = function(
      to,
      subject,
      body,
      is_html = FALSE,
      attachments = NULL
    ) {
      captured_to <<- to
      invisible(NULL)
    },
    {
      NotifyError("An error occurred")
      expect_equal(captured_to, "error@myfirepulse.com")
    }
  )
})

test_that("NotifyError always sends as HTML", {
  set_graph_envvars()
  Sys.setenv(ERROR_EMAIL = "error@myfirepulse.com")

  captured_is_html <- NULL

  with_mocked_bindings(
    SendEmail = function(
      to,
      subject,
      body,
      is_html = FALSE,
      attachments = NULL
    ) {
      captured_is_html <<- is_html
      invisible(NULL)
    },
    {
      NotifyError("An error occurred")
      expect_true(captured_is_html)
    }
  )
})

test_that("NotifyError skips SendEmail when ERROR_EMAIL is not set", {
  set_graph_envvars()
  Sys.unsetenv("ERROR_EMAIL")

  send_email_called <- FALSE

  with_mocked_bindings(
    SendEmail = function(...) {
      send_email_called <<- TRUE
      invisible(NULL)
    },
    {
      expect_no_error(NotifyError("An error occurred"))
      expect_false(send_email_called)
    }
  )
})
