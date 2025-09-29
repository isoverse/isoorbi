# factor_in_order() ====================

test_that("factor_in_order()", {
  # text factor
  x <- c("b", "b", "d", "b", "d", "a", "f", "a", "a", "b", "c")
  expect_s3_class(factor_in_order(x), "factor")
  expect_equal(factor_in_order(x) |> levels(), c("b", "d", "a", "f", "c"))
  expect_equal(factor_in_order(x) |> as.character(), x)
  expect_equal(factor_in_order(factor(x)), factor_in_order(x))

  # number factor
  y <- c(4, 3, 1, 3, 5, 5, 2, 1)
  expect_s3_class(factor_in_order(y), "factor")
  expect_equal(factor_in_order(y) |> levels(), c("4", "3", "1", "5", "2"))
  expect_equal(factor_in_order(y) |> as.character(), as.character(y))
  expect_equal(factor_in_order(factor(y)), factor_in_order(y))
})

# check_arg() ================

test_that("check_arg()", {
  # errors
  test <- function(my_x, include_type = TRUE) {
    check_arg(
      my_x,
      !missing(my_x) && is_scalar_character(my_x),
      "must be a character",
      include_type = include_type
    )
  }
  expect_error(test(), "my_x.*must be a character.*not missing")
  expect_error(test(include_type = FALSE), "my_x.*must be a character$")
  expect_error(test(2), "my_x.*must be a character.*not a number")
  expect_error(test(include_type = FALSE), "my_x.*must be a character$")
  test2 <- function(my_x = "test") {
    check_arg(my_x, !missing(my_x) && is_bool(my_x), "must be logical")
  }
  expect_error(test2(), "my_x.*must be logical.*not a string")
})

# check_tibble() ================

test_that("check_tibble()", {
  # errors
  test <- function(my_df) check_tibble(my_df, c("dne"))
  test() |> expect_error("my_df.*must be a data frame.*not missing")
  test(42) |> expect_error("my_df.*must be a data frame.*not a number")
  check_tibble(mtcars, c("dne", "dne2")) |> expect_error("dne2.*are missing")
  check_tibble(mtcars, c("mpg", "disp")) |> expect_silent()
  check_tibble(mtcars, c("mpg", "dne|disp")) |>
    expect_error("dne\\|disp.*missing")
  check_tibble(mtcars, c("mpg", "dne|disp"), regexps = TRUE) |> expect_silent()
})

# start_info() / finish_info() ================

test_that("start_info() / finish_info()", {
  # testing functions
  wrap_f <- function() {
    warning("my warning")
    return(my_f())
  }
  my_f <- function() {
    internal_func <- function() {
      abort("my error")
    }
    internal_func()
  }
  conditions <- try_catch_cnds(wrap_f())$conditions
  expect_s3_class(conditions, "data.frame")

  # cli tests --> this is nested instead of at the top level (with test_that_cli)
  # so that the positron testing framework recognizes the test exists
  test_that_cli(
    "start/finish_info()",
    configs = c("plain", "fancy"),
    {
      # testing function
      test_info <- function(
        ...,
        keep = FALSE,
        func = TRUE,
        time = FALSE, # otherwise snapshots are always different
        conditions = tibble(),
        delay = 0.1
      ) {
        x <- 5
        start <- start_info(
          "testing {.field x = {x}}",
          keep = keep,
          func = func
        )
        Sys.sleep(delay)
        finish_info(
          "done with {.field x = {x}}",
          start = start,
          func = func,
          conditions = conditions,
          time = time,
          ...
        )
      }

      # default
      test_info() |> expect_snapshot()

      # with permanent starting message
      test_info(keep = TRUE) |> expect_snapshot()

      # without function
      test_info(keep = TRUE, func = FALSE) |> expect_snapshot()

      # with issues
      test_info(conditions = conditions) |> expect_snapshot()

      # with issues but not showing the details
      test_info(conditions = conditions, show_conditions = FALSE) |>
        expect_snapshot()

      # with a single warning
      test_info(conditions = conditions[1, ]) |> expect_snapshot()

      # with a single error
      test_info(conditions = conditions[2, ]) |> expect_snapshot()

      # abort warnings
      test_info(conditions = conditions, abort_if_warnings = TRUE) |>
        expect_snapshot(error = TRUE)

      # abort errors
      test_info(conditions = conditions, abort_if_errors = TRUE) |>
        expect_snapshot(error = TRUE)
    }
  )
})
