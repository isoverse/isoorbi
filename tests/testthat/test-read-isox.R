# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

# orbi_find_isox() ============

test_that("orbi_find_isox()", {
  # safety checks
  orbi_find_isox() |>
    expect_error("must point to an existing directory")
  orbi_find_isox(42) |> expect_error("must point to an existing directory")
  orbi_find_isox(c(42, "DNE")) |>
    expect_error("must point to existing directories")
})

# orbi_read_isox() ============

test_that("orbi_read_isox()", {
  # safety checks
  orbi_read_isox() |> expect_error("must be a file path")
  orbi_read_isox(character()) |> expect_error("must be a file path")
  orbi_read_isox(42) |> expect_error("must be a file path")
  orbi_read_isox("DNE") |> expect_error("does not exist")
  orbi_read_isox(c("DNE", "DNE2")) |> expect_error("do not exist")

  # wrong extension
  temp_file <- tempfile(fileext = ".wrong")
  cat("empty", file = temp_file) # create the temp file
  orbi_read_isox(temp_file) |> expect_error("unrecognized file extension")
  unlink(temp_file) # destroy the temp file

  # read files
  test_that_cli(
    "orbi_read_isox()",
    configs = c("plain", "fancy"),
    {
      # corrupt files - missing column
      orbi_read_isox(file.path(
        base_dir,
        "test_files",
        "missing_column.isox"
      )) |>
        expect_snapshot(error = TRUE)

      # read single file
      expect_snapshot({
        df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox"))
      })

      # check output
      expect_equal(
        names(df),
        c(
          "filepath",
          "filename",
          "scan.no",
          "time.min",
          "compound",
          "isotopocule",
          "ions.incremental",
          "tic",
          "it.ms"
        )
      )

      # number of rows
      expect_equal(nrow(df), 5184)

      # read multiple files
      expect_snapshot({
        df2 <-
          orbi_get_example_files(c(
            "testfile_dual_inlet.isox",
            "testfile_flow.isox"
          )) |>
          orbi_read_isox()
      })

      # check result
      expect_equal(nrow(df2), 11633)
    }
  ) |>
    withr::with_options(new = list(show_exec_times = FALSE))
})

# orbi_simplify_isox() =========

test_that("test orbi_simplify_isox()", {
  #  test file
  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    suppressMessages()

  # safety checks
  expect_error(orbi_simplify_isox(), "need a `dataset` data frame")
  expect_error(
    orbi_simplify_isox(dataset = "string"),
    "need a `dataset` data frame"
  )
  expect_type(df, "list")
  dataset = subset(df, select = -c(filename))
  expect_error(
    orbi_simplify_isox(dataset),
    "dataset` requires columns `filepath`, `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`"
  )
  dataset = df[0, ]
  expect_error(orbi_simplify_isox(dataset), "dataset contains no rows")

  expect_error(
    orbi_simplify_isox(),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  expect_error(
    orbi_simplify_isox(dataset = as.matrix(df)),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  expect_error(
    orbi_simplify_isox(dataset = df[, 1:5]),
    "dataset` requires columns `filepath`, `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`"
  )
  dataset = df[0, ]
  expect_error(orbi_simplify_isox(dataset), "dataset contains no rows")

  df2 <- df |> mutate(dummy = "1") |> select(-scan.no)
  expect_error(
    orbi_simplify_isox(dataset = df2),
    "`dataset` requires columns `filepath`, `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`",
    fixed = TRUE
  )

  # success
  expect_message(
    {
      df <- orbi_simplify_isox(df)
    },
    "kept columns"
  )
  expect_equal(nrow(df), 5184L)
})

# orbi_filter_files() ===========

test_that("test orbi_filter_files()", {
  # success
  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    suppressMessages()

  expect_true(is.tbl(orbi_filter_files(df))) |> suppressMessages()

  expect_true(is.tbl(orbi_filter_files(
    df,
    filenames = "20220125_01",
    compounds = "NO3-",
    isotopocules = c("15N", "18O"),
    time_min = 0.5,
    time_max = 2
  ))) |>
    suppressMessages()

  # failure
  expect_error(orbi_filter_files(), "must be.*aggregated.*or.*data frame")

  expect_error(
    orbi_filter_files(dataset = as.matrix(df)),
    "must be.*aggregated.*or.*data frame"
  )

  expect_error(
    orbi_filter_files(dataset = df, time_min = "A"),
    "`time_min` must be a single number (or NULL)",
    fixed = TRUE
  )

  expect_error(
    orbi_filter_files(dataset = df, time_max = "A"),
    "`time_max` must be a single number (or NULL)",
    fixed = TRUE
  )

  expect_error(
    orbi_filter_files(dataset = df, time_min = c(0.1, 0.2)),
    "`time_min` must be a single number (or NULL)",
    fixed = TRUE
  )

  expect_error(
    orbi_filter_files(dataset = df, time_max = c(0.1, 0.2)),
    "`time_max` must be a single number (or NULL)",
    fixed = TRUE
  )

  expect_error(
    orbi_filter_files(dataset = df, filenames = as.matrix(c(1, 0))),
    "`filenames` must be a vector of filenames (or NULL)",
    fixed = TRUE
  )

  expect_error(
    orbi_filter_files(dataset = df, isotopocules = as.matrix(c(1, 0))),
    "`isotopocules` must be a vector of isotopocules (or NULL)",
    fixed = TRUE
  )

  expect_error(
    orbi_filter_files(dataset = df, compounds = as.matrix(c(1, 0))),
    "`compounds` must be a vector of compounds (or NULL)",
    fixed = TRUE
  )
})
