test_that("orbi_get_problems()", {
  # safety checks
  expect_error(
    orbi_get_problems(),
    "must be a list or data frame",
    fixed = TRUE
  )
  expect_error(
    orbi_get_problems(42),
    "must be a list or data frame",
    fixed = TRUE
  )
  expect_error(
    orbi_get_problems("42"),
    "must be a list or data frame",
    fixed = TRUE
  )

  # problems from list
  test <- list(problems = tibble(type = c("warning", "error")))
  expect_equal(
    orbi_get_problems(test),
    tibble(
      type = c("warning", "error"),
      call = NA_character_,
      message = NA_character_,
      condition = list(NULL)
    )
  )
  expect_equal(
    test |> orbi_get_problems(),
    list(conditions = tibble(type = c("warning", "error"))) |>
      orbi_get_problems()
  )

  # can get problems from nested data frames
  test <- tibble(
    problems = list(tibble(type = "warning"), tibble(type = "error"))
  )
  expect_equal(
    orbi_get_problems(test),
    tibble(
      type = c("warning", "error"),
      call = NA_character_,
      message = NA_character_,
      condition = list(NULL)
    )
  )

  # attribute (like in readr) takes precedence
  attr(test, "problems") <- tibble(message = "other")
  expect_equal(
    orbi_get_problems(test),
    tibble(
      type = c("warning", "error", NA),
      call = NA_character_,
      message = c(NA, NA, "other"),
      condition = list(NULL)
    )
  )
})
