# testing functions
wrap_f <- function(show_message = FALSE) {
  warning("we're here!")
  rlang::warn("no context")
  return(my_f(show_message = show_message))
}

my_f <- function(show_message = FALSE) {
  if (show_message) {
    message("let's get started")
  }
  warning(
    "long trouble! This sentences is easily longer than a single line in standard line width and therefore needs a sensible linebreak somewhere along the way."
  )
  internal_func <- function() {
    rlang::abort("oh no internal {value} error!")
  }
  internal_func()
  return("success")
}

# try_catch_cnds() ==========

test_that("try_catch_cnds()", {
  # with truncated callstack (default)
  try_catch_cnds(wrap_f())$conditions$condition[[4]]$trace |>
    as.data.frame() |>
    expect_snapshot()

  # without truncated callstack
  try_catch_cnds(wrap_f(), truncate_call_stack = FALSE)$conditions$condition[[
    4
  ]]$trace |>
    as.data.frame() |>
    expect_snapshot()

  # with message
  try_catch_cnds(wrap_f(show_message = TRUE)) |>
    expect_message("let's get started")

  # don't catch error
  try_catch_cnds(wrap_f(), catch_errors = FALSE) |>
    expect_error("error")

  # don't catch warnings
  try_catch_cnds(wrap_f(), catch_warnings = FALSE) |>
    expect_warning("trouble") |>
    suppressWarnings()

  # function output
  out <- try_catch_cnds(wrap_f())
  expect_equal(out$result, NULL)
  expect_s3_class(out$conditions, "tbl")
  expect_snapshot(out$conditions)

  # with a specific error return value
  out_fail_retval <- try_catch_cnds(wrap_f(), error_value = FALSE)
  expect_equal(out_fail_retval$result, FALSE)
  expect_equal(out$conditions, out_fail_retval$conditions)
})

# summarize_cnds() ==========

test_that("summarize_cnds()", {
  # conditions to test with
  out <- try_catch_cnds(wrap_f())
  expect_s3_class(out$conditions, "data.frame")

  # cli tests
  test_that_cli(
    "summarize_cnds()",
    configs = c("plain", "fancy"),
    {
      # default outside a function
      summarize_cnds(out$conditions, .call = NULL) |>
        cli_bullets() |>
        expect_snapshot()

      # inside a function
      test_summarize_cnds <- function(..., conditions = out$conditions) {
        summarize_cnds(conditions, ...) |>
          cli_bullets()
      }

      # default (inside function)
      test_summarize_cnds() |> expect_snapshot()

      # without symbol
      test_summarize_cnds(include_symbol = FALSE) |> expect_snapshot()

      # without call
      test_summarize_cnds(include_call = FALSE) |> expect_snapshot()

      # warnings only
      test_summarize_cnds(conditions = out$conditions[1:3, ]) |>
        expect_snapshot()

      # without issues
      test_summarize_cnds(conditions = out$conditions[c(), ]) |>
        expect_snapshot()

      # with custom call format
      test_summarize_cnds(call_format = "hey {.fn {call}}! ") |>
        expect_snapshot()

      # with message
      test_summarize_cnds(message = "and we get more {fun}") |>
        expect_snapshot()

      # with cli message
      test_summarize_cnds(
        message = format_inline("so this is {.field field}")
      ) |>
        expect_snapshot()

      # with custom summary format
      test_summarize_cnds(
        summary_format = "{message} = {issues} {.strong yeah!}"
      ) |>
        expect_snapshot()
    }
  )
})

# format_cnds() ==========

test_that("format_cnds()", {
  # conditions to test with
  out <- try_catch_cnds(wrap_f())
  expect_s3_class(out$conditions, "data.frame")

  # cli tests
  test_that_cli(
    "format_cnds()",
    configs = c("plain", "fancy"),
    {
      # default
      format_cnds(out$conditions) |> expect_snapshot()

      # without symbol
      format_cnds(out$conditions, include_symbol = FALSE) |> expect_snapshot()

      # without call
      format_cnds(out$conditions, include_call = FALSE) |> expect_snapshot()

      # with custom call format
      format_cnds(out$conditions, call_format = "hey {.fn {call}}! ") |>
        expect_snapshot()

      # with indentation
      format_cnds(out$conditions, indent = 1) |>
        expect_snapshot()

      # with larger indentation
      format_cnds(out$conditions, indent = 3) |>
        expect_snapshot()

      # with prefix
      format_cnds(out$conditions, prefix = "an {x} ") |>
        expect_snapshot()

      # with cli formatted prefix
      format_cnds(out$conditions, prefix = format_inline("{.field x} = ")) |>
        expect_snapshot()
    }
  )
})

# summarize_and_format_cnds() =========

test_that("summarize_and_format_cnds()", {
  # conditions to test with
  out <- try_catch_cnds(wrap_f())
  expect_s3_class(out$conditions, "data.frame")

  # errors
  expect_error(summarize_and_format_cnds(), "conditions.*must be provided")

  # cli tests
  test_that_cli(
    "summarize_and_format_cnds()",
    configs = c("plain", "fancy"),
    {
      # default outside a function
      summarize_and_format_cnds(out$conditions, .call = NULL) |>
        cli_bullets() |>
        expect_snapshot()

      # inside a function
      test_summarize_and_format_cnds <- function(
        ...,
        conditions = out$conditions
      ) {
        summarize_and_format_cnds(conditions, ...) |>
          cli_bullets()
      }

      # default (inside function)
      test_summarize_and_format_cnds() |> expect_snapshot()

      # without symbol
      test_summarize_and_format_cnds(include_symbol = FALSE) |>
        expect_snapshot()

      # without summary
      test_summarize_and_format_cnds(include_summary = FALSE) |>
        expect_snapshot()

      # without call
      test_summarize_and_format_cnds(include_call = FALSE) |> expect_snapshot()

      # warnings only
      test_summarize_and_format_cnds(conditions = out$conditions[1:3, ]) |>
        expect_snapshot()

      # single issue
      test_summarize_and_format_cnds(conditions = out$conditions[1, ]) |>
        expect_snapshot()

      # without issues
      test_summarize_and_format_cnds(conditions = out$conditions[c(), ]) |>
        expect_snapshot()

      # don't show cnds
      test_summarize_and_format_cnds(include_cnds = FALSE) |> expect_snapshot()

      # with message
      test_summarize_and_format_cnds(message = "even more {fun}") |>
        expect_snapshot()

      # with cli message
      test_summarize_and_format_cnds(
        message = format_inline("another {.field field}")
      ) |>
        expect_snapshot()

      # with custom summary format
      test_summarize_and_format_cnds(
        summary_format = "{message} = {issues} {.strong yeah!}"
      ) |>
        expect_snapshot()

      # without cnd calls
      test_summarize_and_format_cnds(nclude_cnd_calls = FALSE) |>
        expect_snapshot()

      # without cnd indentation
      test_summarize_and_format_cnds(out$conditions, indent_cnds = FALSE) |>
        expect_snapshot()
    }
  )
})

# show_cnds() =========

test_that("show_cnds()", {
  # conditions to test with
  out <- try_catch_cnds(wrap_f())
  expect_s3_class(out$conditions, "data.frame")

  # cli tests
  test_that_cli(
    "show_cnds()",
    configs = c("plain", "fancy"),
    {
      # default outside a function
      show_cnds(out$conditions, call = NULL) |> expect_snapshot()

      # inside a function
      test_show_cnds <- function(
        ...,
        conditions = out$conditions
      ) {
        show_cnds(conditions, ...)
      }

      # default (inside function)
      test_show_cnds() |> expect_snapshot()

      # no conditions
      test_show_cnds(conditions = out$conditions[c(), ]) |> expect_silent()

      # could test other cases but it's redundant with the summarize_and_format_cnds() testsß
    }
  )
})


# abort_cnds() =========

test_that("abort_cnds()", {
  # conditions to test with
  out <- try_catch_cnds(wrap_f())
  expect_s3_class(out$conditions, "data.frame")

  # cli tests
  test_that_cli(
    "abort_cnds()",
    configs = c("plain", "fancy"),
    {
      # default outside a function
      abort_cnds(out$conditions, call = NULL) |> expect_snapshot(error = TRUE)

      # inside a function
      test_abort_cnds <- function(
        ...,
        conditions = out$conditions
      ) {
        abort_cnds(conditions, ...)
      }

      # default (inside function)
      test_abort_cnds() |> expect_snapshot(error = TRUE)

      # no conditions
      test_abort_cnds(conditions = out$conditions[c(), ]) |> expect_silent()

      # could test other cases but it's redundant with the summarize_and_format_cnds() testsß
    }
  )
})
