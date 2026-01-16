# Test Suite for CheckWriteResult Function
# Run with: testthat::test_file("test_CheckWriteResult.R")

library(testthat)
library(logger)
library(glue)

# Mock shinyalert for testing
mockShinyalert <- function() {
  # Store calls to shinyalert for verification
  calls <- list()

  shinyalert <- function(title, text, type, closeOnClickOutside = TRUE) {
    calls <<- c(
      calls,
      list(list(
        title = title,
        text = text,
        type = type,
        closeOnClickOutside = closeOnClickOutside
      ))
    )
  }

  list(
    shinyalert = shinyalert,
    getCalls = function() calls,
    reset = function() calls <<- list()
  )
}

# Test successful write with default parameters
test_that("CheckWriteResult returns TRUE for successful write", {
  mock <- mockShinyalert()

  # Temporarily replace shinyalert
  with_mocked_bindings(
    {
      result <- CheckWriteResult(1)

      expect_true(result)

      # Verify success modal was shown
      calls <- mock$getCalls()
      expect_length(calls, 1)
      expect_equal(calls[[1]]$title, "Success")
      expect_equal(calls[[1]]$type, "success")
    },
    shinyalert = mock$shinyalert,
    .package = "shinyalert"
  )
})

# Test successful write with custom message
test_that("CheckWriteResult displays custom success message", {
  mock <- mockShinyalert()

  with_mocked_bindings(
    {
      result <- CheckWriteResult(1, successMessage = "Training record added!")

      expect_true(result)

      calls <- mock$getCalls()
      expect_equal(calls[[1]]$text, "Training record added!")
    },
    shinyalert = mock$shinyalert,
    .package = "shinyalert"
  )
})

# Test silent success (showMessage = FALSE)
test_that("CheckWriteResult skips modal when showMessage = FALSE", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        result <- CheckWriteResult(1, showMessage = FALSE)

        expect_true(result)

        # Verify no modal was shown
        calls <- mock$getCalls()
        expect_length(calls, 0)
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test failure with NA result
test_that("CheckWriteResult returns FALSE for NA result", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        result <- CheckWriteResult(NA)

        expect_false(result)

        # Verify error modal was shown
        calls <- mock$getCalls()
        expect_length(calls, 1)
        expect_equal(calls[[1]]$title, "Error")
        expect_equal(calls[[1]]$type, "error")
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test failure with result below minimum
test_that("CheckWriteResult returns FALSE when result below expectedMin", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        result <- CheckWriteResult(0, expectedMin = 1, expectedMax = 5)

        expect_false(result)

        calls <- mock$getCalls()
        expect_match(calls[[1]]$text, "Result: 0")
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test failure with result above maximum
test_that("CheckWriteResult returns FALSE when result above expectedMax", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        result <- CheckWriteResult(10, expectedMin = 1, expectedMax = 5)

        expect_false(result)

        calls <- mock$getCalls()
        expect_match(calls[[1]]$text, "Result: 10")
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test with context in error message
test_that("CheckWriteResult includes context in error messages", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        result <- CheckWriteResult(NA, context = "inserting training record")

        expect_false(result)

        calls <- mock$getCalls()
        expect_match(calls[[1]]$text, "when inserting training record")
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test success within range
test_that("CheckWriteResult accepts results within range", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        # Test minimum boundary
        result1 <- CheckWriteResult(
          5,
          expectedMin = 5,
          expectedMax = 10,
          showMessage = FALSE
        )
        expect_true(result1)

        # Test maximum boundary
        result2 <- CheckWriteResult(
          10,
          expectedMin = 5,
          expectedMax = 10,
          showMessage = FALSE
        )
        expect_true(result2)

        # Test middle of range
        result3 <- CheckWriteResult(
          7,
          expectedMin = 5,
          expectedMax = 10,
          showMessage = FALSE
        )
        expect_true(result3)
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test zero rows affected (valid for deletes)
test_that("CheckWriteResult handles zero rows when expectedMin = 0", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        result <- CheckWriteResult(
          0,
          expectedMin = 0,
          expectedMax = 100,
          showMessage = FALSE
        )
        expect_true(result)
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test input validation
test_that("CheckWriteResult validates inputs correctly", {
  # Missing result
  expect_error(
    CheckWriteResult(),
    "'result' must be provided"
  )

  # Invalid successMessage
  expect_error(
    CheckWriteResult(1, successMessage = c("msg1", "msg2")),
    "'successMessage' must be a single character string"
  )

  # Invalid context
  expect_error(
    CheckWriteResult(1, context = c("ctx1", "ctx2")),
    "'context' must be NULL or a single character string"
  )

  # Non-numeric expected values
  expect_error(
    CheckWriteResult(1, expectedMin = "1"),
    "'expectedMin' and 'expectedMax' must be numeric"
  )

  # expectedMin > expectedMax
  expect_error(
    CheckWriteResult(1, expectedMin = 10, expectedMax = 5),
    "'expectedMin' must be less than or equal to 'expectedMax'"
  )

  # Invalid showMessage
  expect_error(
    CheckWriteResult(1, showMessage = "yes"),
    "'showMessage' must be TRUE or FALSE"
  )
})

# Test non-numeric result
test_that("CheckWriteResult handles non-numeric result", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        result <- CheckWriteResult("not a number")

        expect_false(result)

        calls <- mock$getCalls()
        expect_equal(calls[[1]]$type, "error")
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test large batch operation
test_that("CheckWriteResult handles large batch operations", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        result <- CheckWriteResult(
          500,
          successMessage = "Batch update complete",
          context = "updating multiple records",
          expectedMin = 100,
          expectedMax = 1000,
          showMessage = FALSE
        )

        expect_true(result)
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})

# Test context formatting
test_that("CheckWriteResult formats context correctly", {
  mock <- mockShinyalert()

  suppressMessages({
    log_threshold(FATAL)

    with_mocked_bindings(
      {
        # With context
        result1 <- CheckWriteResult(NA, context = "deleting records")
        calls1 <- mock$getCalls()
        expect_match(calls1[[1]]$text, "when deleting records")

        mock$reset()

        # Without context
        result2 <- CheckWriteResult(NA, context = NULL)
        calls2 <- mock$getCalls()
        expect_false(grepl("when", calls2[[1]]$text))
      },
      shinyalert = mock$shinyalert,
      .package = "shinyalert"
    )

    log_threshold(INFO)
  })
})
