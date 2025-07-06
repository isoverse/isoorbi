test_that("try_catch_cnds() works", {
  # testing functions
  wrap_f <- function() {
    warning("we're here!")
    rlang::warn("no context")
    return(my_f())
  }

  my_f <- function() {
    message("let's get started")
    warning("trouble!")
    internal_func <- function() {
      rlang::abort("oh no internal error!")
    }
    internal_func()
    return("success")
  }

  # tests
  expect_message(out <- try_catch_cnds(wrap_f()), "let's get started")
  expect_equal(out$result, NULL)
  expect_s3_class(out$conditions, "tbl")
  expect_error(try_catch_cnds(wrap_f(), catch_errors = FALSE), "error") |>
    suppressMessages()
  expect_warning(
    out2 <- try_catch_cnds(
      wrap_f(),
      catch_warnings = FALSE,
      error_value = FALSE
    ),
    "trouble"
  ) |>
    suppressWarnings() |>
    suppressMessages()
  expect_equal(out2$result, FALSE)

  # resulting messages
  expect_snapshot(show_cnds(out$conditions))
  expect_snapshot(show_cnds(
    out$conditions,
    include_call = FALSE,
    summary_message = "hello {.pkg isoorbi}"
  ))
  expect_snapshot(show_cnds(
    out$conditions,
    include_summary = FALSE,
    include_cnd_calls = FALSE
  ))
})
