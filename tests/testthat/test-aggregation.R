test_that("orbi_start_aggregator()", {
  # errors
  expect_error(orbi_start_aggregator(), "must be a string")

  # value
  expect_true(is(orbi_start_aggregator("test"), "orbi_aggregator"))

  # messages
  test_that_cli("orbi_start_aggregator()", configs = c("plain", "fancy"), {
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
  test_that_cli("orbi_start_aggregator()", configs = c("plain", "fancy"), {
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
  test_that_cli("get_data()", configs = c("plain", "fancy"), {
    expect_snapshot(out <- test_run1())
    expect_snapshot(out <- test_run2())
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))

  # data
  expect_snapshot_value(test_run1(), style = "json2")
  expect_snapshot_value(test_run2(), style = "json2")
})
