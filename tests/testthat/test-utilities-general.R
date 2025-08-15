# check_arg() ================

test_that("test check_arg()", {
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

test_that("test check_tibble()", {
  # errors
  test <- function(my_df) check_tibble(my_df, c("dne"))
  expect_error(test(), "my_df.*must be a data frame.*not missing")
  expect_error(test(42), "my_df.*must be a data frame.*not a number")
  test <- function(my_df) check_tibble(my_df, c("dne", "dne2"))
  expect_error(test(mtcars), "dne2.*are missing")
})

# start_info() / finish_info() ================

test_that("test start_info() / finish_info()", {
  test <- function(keep = FALSE, func = TRUE) {
    start <- start_info("testing", keep = keep, func = func)
    Sys.sleep(0.1)
    finish_info("done", start = start)
  }

  # default: disappearing start info (how to test for the disappearence?)
  test() |>
    expect_message("test().*testing...") |>
    expect_message("test().*done") |>
    suppressMessages()

  # keep start message
  test(TRUE) |>
    expect_message("test().*testing...") |>
    expect_message("test().*done")

  # do not include function
  test(TRUE, FALSE) |>
    expect_message("testing...") |>
    expect_message("done") |>
    suppressMessages()
})
