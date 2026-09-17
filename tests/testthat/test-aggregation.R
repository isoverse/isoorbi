test_that("orbi_start_aggregator()", {
  # errors
  expect_error(orbi_start_aggregator(), "must be a string")

  # value
  expect_true(is(orbi_start_aggregator("test"), "orbi_aggregator"))

  # messages
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot(orbi_start_aggregator("test"))
  })
})

test_that("orbi_add_to_aggregator()", {
  # errors
  orbi_add_to_aggregator(42) |>
    expect_error("must be.*orbi_aggregator.*tibble")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator() |>
    expect_error("dataset.*must be a string")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data") |>
    expect_error("column.*must be a string")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data", "col", 42) |>
    expect_error("source.*must be.*character.*or list")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data", "col", regexp = 42) |>
    expect_error("regexp.*must be TRUE or FALSE")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data", "col", cast = 42) |>
    expect_error("cast.*must be a string")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data", "col", func = 42) |>
    expect_error("func.*must be a string")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data", "col", args = 42) |>
    expect_error("args.*must be a list")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data", "col", cast = "DNE") |>
    expect_error("function.*could not be found")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data", "col", func = "DNE") |>
    expect_error("function.*could not be found")
  orbi_start_aggregator("test") |>
    orbi_add_to_aggregator("data", "col", cast = "sqrt", default = "x") |>
    expect_error()

  # values
  expect_equal(
    orbi_start_aggregator("test") |>
      orbi_add_to_aggregator("data", "col") |> # overwritten in next
      orbi_add_to_aggregator("data", "col", cast = "as.integer"),
    orbi_start_aggregator("test") |>
      orbi_add_to_aggregator("data", "col", cast = "as.integer")
  )

  # messages
  test_that_cli("cli", configs = c("plain", "fancy"), {
    orbi_start_aggregator("test") |>
      orbi_add_to_aggregator("data", "col") |>
      orbi_add_to_aggregator("data", "num", cast = "as.integer") |>
      orbi_add_to_aggregator(
        "data",
        "new",
        source = c("def", "alt def"),
        default = 4
      ) |>
      orbi_add_to_aggregator(
        "data",
        "w\\1_\\2",
        "(\\d+)-(.*)",
        regexp = TRUE
      ) |>
      orbi_add_to_aggregator(
        "data",
        "from_fun",
        cast = "as.integer",
        source = list(c("a", "b"), "x"),
        func = "mean"
      ) |>
      expect_snapshot()
  })
})

test_that("orbi_register_aggregator() and orbi_get_aggregator()", {
  # keep the aggregator registered below from leaking into other tests
  # (it would otherwise show up in the orbi_get_options() snapshot)
  withr::local_options(list(
    isoorbi.aggregators = orbi_get_option("aggregators")
  ))

  # errors
  orbi_register_aggregator(42) |>
    expect_error("must be.*orbi_aggregator.*tibble")
  orbi_start_aggregator("test") |>
    orbi_register_aggregator(42) |>
    expect_error("name.*a string")
  orbi_get_aggregator("dne") |>
    expect_error("not.*registered")

  # values
  agg <- orbi_start_aggregator("test") |> orbi_add_to_aggregator("ds", "col")
  agg |> orbi_register_aggregator()
  expect_equal(orbi_get_option("aggregators")$test, agg)
  expect_equal(orbi_get_aggregator("test"), agg)
})

test_that("get_data()", {
  # errors
  get_data() |> expect_error("must be.*list")
  get_data(42) |> expect_error("must be.*list")
  get_data(list()) |> expect_error("at least one")
  get_data(list(a = tibble()), by = 42) |> expect_error("must be.*character")
  get_data(list(a = tibble())) |> expect_error("no.*selections")
  get_data(list(a = tibble()), b = "a") |>
    expect_error("dataset.*not in the data")
  get_data(list(a = tibble(), d = tibble()), a = "a") |>
    expect_error("error selecting columns")
  get_data(list(a = cars, b = cars), a = "speed", b = "dist") |>
    expect_error("unclear how to join")
  get_data(
    list(a = cars, b = cars |> mutate(speed = as.character(speed))),
    a = "speed",
    b = "dist",
    by = "speed"
  ) |>
    expect_error("encountered issue when joining")
  get_data(
    list(a = cars, b = cars),
    a = everything(),
    b = everything(),
    by = "speed"
  ) |>
    expect_error("encountered issue") # many-to-many relationship

  # working snapshots

  test_run1 <- function() {
    list(
      a = tibble(id = c("a", "b"), info = paste(id, "info")),
      b = tibble(id = "a", x = 1:10, y = 42),
      data = tibble(id = "a", x = 1:10, z = x * 10)
    ) |>
      get_data(
        a = everything(),
        b = c("id", "x"),
        data = everything(),
        by = c("id", "x")
      )
  }

  test_run2 <- function() {
    get_data(
      list(a = cars, b = cars),
      a = everything(),
      b = everything(),
      by = "speed",
      relationship = "many-to-many"
    )
  }

  # messages
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot(out <- test_run1())
    expect_snapshot(out <- test_run2())
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))

  # data
  expect_snapshot_value(test_run1(), style = "json2") |>
    suppressMessages()
  expect_snapshot_value(test_run2(), style = "json2") |>
    suppressMessages()
})

# peak flags ==========

test_that("orbi_peak_flags_to_text()", {
  # errors
  orbi_peak_flags_to_text() |> expect_error()

  # individual flags, decoded from their bitmask value
  expect_equal(orbi_peak_flags_to_text(0L), "none")
  expect_equal(orbi_peak_flags_to_text(1L), "saturated")
  expect_equal(orbi_peak_flags_to_text(2L), "fragmented")
  expect_equal(orbi_peak_flags_to_text(4L), "merged")
  expect_equal(orbi_peak_flags_to_text(8L), "exception")
  expect_equal(orbi_peak_flags_to_text(16L), "reference")
  expect_equal(orbi_peak_flags_to_text(32L), "modified")
  expect_equal(orbi_peak_flags_to_text(64L), "lock peak")

  # combinations are alphabetical and joined with " + " (NOT in bit order)
  expect_equal(orbi_peak_flags_to_text(10L), "exception + fragmented")
  expect_equal(orbi_peak_flags_to_text(18L), "fragmented + reference")
  expect_equal(orbi_peak_flags_to_text(80L), "lock peak + reference")
  expect_equal(
    orbi_peak_flags_to_text(88L),
    "exception + lock peak + reference"
  )
  expect_equal(
    orbi_peak_flags_to_text(127L),
    "exception + fragmented + lock peak + merged + modified + reference + saturated"
  )

  # vectorized, incl. missing values
  expect_equal(
    orbi_peak_flags_to_text(c(0L, 8L, NA_integer_, 10L)),
    c("none", "exception", NA_character_, "exception + fragmented")
  )
  expect_equal(orbi_peak_flags_to_text(integer(0)), character(0))

  # doubles are fine too
  expect_equal(orbi_peak_flags_to_text(c(0, 16)), c("none", "reference"))
})

test_that("orbi_peak_flags_include()", {
  # errors
  orbi_peak_flags_include(0L) |> expect_error("must be at least one flag name")
  orbi_peak_flags_include(0L, 42) |> expect_error("must be at least one flag")
  orbi_peak_flags_include(0L, character(0)) |>
    expect_error("must be at least one flag")
  orbi_peak_flags_include(0L, "DNE") |> expect_error("unknown peak flag")
  orbi_peak_flags_include(0L, c("reference", "DNE")) |>
    expect_error("unknown peak flag")
  # "none" is not a bit in the bitmask, the user is pointed at the column instead
  orbi_peak_flags_include(0L, "none") |>
    expect_error("not a flag that a peak's flags can include")
  orbi_peak_flags_include(0L, c("none", "reference")) |>
    expect_error("not a flag that a peak's flags can include")
  # neither a bitmask nor the decoded text
  orbi_peak_flags_include(TRUE, "reference") |>
    expect_error("must be the decoded peak flags or the raw numeric bitmask")

  # single flag: the peak has to carry it, additional flags are allowed
  flags <- c(0L, 8L, 16L, 18L, 24L, 64L, 80L)
  expect_equal(
    orbi_peak_flags_include(flags, "reference"),
    c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    orbi_peak_flags_include(flags, "lock peak"),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )

  # several flags: ALL of them have to be present
  expect_equal(
    orbi_peak_flags_include(flags, c("reference", "exception")),
    c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  # 26 = reference + exception + fragmented, i.e. still includes both of them
  expect_true(orbi_peak_flags_include(26L, c("reference", "exception")))

  # missing values are propagated
  expect_equal(orbi_peak_flags_include(NA_integer_, "reference"), NA)
  expect_equal(orbi_peak_flags_include(NA_character_, "reference"), NA)

  # the decoded text gives the exact same answer as the bitmask, also as a factor
  all_flags <- 0:127
  all_text <- orbi_peak_flags_to_text(all_flags)
  for (flag in list(
    "reference",
    "lock peak",
    "saturated",
    c("reference", "fragmented"),
    c("exception", "fragmented", "merged")
  )) {
    expect_equal(
      orbi_peak_flags_include(all_text, flag),
      orbi_peak_flags_include(all_flags, flag)
    )
    expect_equal(
      orbi_peak_flags_include(factor(all_text), flag),
      orbi_peak_flags_include(all_flags, flag)
    )
  }
})
