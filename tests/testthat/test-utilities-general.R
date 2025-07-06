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

test_that("test check_tibble()", {
  # errors
  test <- function(my_df) check_tibble(my_df, c("dne"))
  expect_error(test(), "my_df.*must be a data frame.*not missing")
  expect_error(test(42), "my_df.*must be a data frame.*not a number")
  test <- function(my_df) check_tibble(my_df, c("dne", "dne2"))
  expect_error(test(mtcars), "dne2.*are missing")
})
