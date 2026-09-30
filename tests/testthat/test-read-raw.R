# Tests: Functions to load, pre-filter and simplify IsoX data

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

test_that("orbi_find_raw()", {
  # safety checks
  expect_error(orbi_find_raw(), "folder.*must point to an existing directory")
  expect_error(orbi_find_raw(42), "folder.*must point to an existing directory")
  expect_error(
    orbi_find_raw(c("DNE", "DNE2")),
    "folder.*must point to existing directories"
  )

  # files included in package
  expect_equal(
    orbi_find_raw(
      system.file("extdata", package = "isoorbi"),
      pattern = "nitrate"
    ) |>
      basename(),
    c("nitrate_test_10scans.raw", "nitrate_test_1scan.raw")
  )

  # create test folder for testing parameters
  test_path <- tempdir() |> file.path("isoorbi")
  test_file1 <- "nitrate_test_1scan.raw"
  test_file2 <- "nitrate_test_10scans.raw"
  unlink(test_path, recursive = TRUE, force = TRUE)
  dir.create(test_path)
  dir.create(file.path(test_path, "sub"))
  file.copy(
    system.file("extdata", test_file1, package = "isoorbi"),
    file.path(test_path, test_file1)
  )
  file.copy(
    system.file("extdata", test_file2, package = "isoorbi"),
    file.path(test_path, "sub", test_file2)
  )

  # tests
  expect_equal(
    orbi_find_raw(test_path, recursive = FALSE),
    file.path(test_path, test_file1)
  )
  expect_equal(
    orbi_find_raw(c(test_path, test_path), recursive = FALSE),
    file.path(test_path, test_file1)
  )
  expect_equal(
    orbi_find_raw(test_path),
    c(file.path(test_path, test_file1), file.path(test_path, "sub", test_file2))
  )

  # cache folders
  writeLines("tmp", file.path(test_path, paste0(test_file1, ".cache.zip")))
  expect_equal(
    # find original file instead of cache folder
    orbi_find_raw(test_path, recursive = FALSE),
    file.path(test_path, test_file1)
  )
  unlink(file.path(test_path, test_file1))
  expect_equal(
    # if original doesn't exist, find cached file
    orbi_find_raw(test_path, recursive = FALSE),
    file.path(test_path, paste0(test_file1, ".cache.zip"))
  )
  expect_equal(
    # expect if instructed not to find cache
    orbi_find_raw(test_path, recursive = FALSE, include_cache = FALSE),
    character(0)
  )

  # cleanup
  unlink(test_path, recursive = TRUE, force = TRUE)
})

test_that("orbi_read_raw()", {
  # safety checks
  expect_error(orbi_read_raw(), "file_paths.*must be at least one")
  expect_error(orbi_read_raw(42), "file_paths.*must be at least one")
  expect_error(orbi_read_raw(character()), "file_paths.*must be at least one")

  # succesful read without spectra (default)
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot(
      x <- system.file("extdata", package = "isoorbi") |>
        orbi_find_raw(pattern = "nitrate") |>
        # read without spectra (on CRAN should read cache so we DONT download isoraw)
        orbi_read_raw(read_cache = TRUE, cache = FALSE)
    )
    expect_snapshot(x)

    # aggregate
    expect_snapshot(y <- orbi_aggregate_raw(x))
    expect_snapshot(y)
    y$file_info$file_path <- NULL # OS dependent
    y$file_info$`Creation date` <- NULL # OS dependent
    expect_snapshot(
      out <- y |> orbi_get_data(scans = everything(), spectra = everything())
    )
  })

  # succesful read with spectra
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot(
      x <- system.file("extdata", package = "isoorbi") |>
        orbi_find_raw(pattern = "nitrate") |>
        # read with spectra (on CRAN should read cache so we DONT download isoraw)
        orbi_read_raw(read_cache = TRUE, cache = FALSE, include_spectra = 1)
    )
    expect_snapshot(x)

    # aggregate
    expect_snapshot(y <- orbi_aggregate_raw(x, aggregator = "extended"))
    expect_snapshot(y)
    expect_snapshot(y <- orbi_aggregate_raw(x, aggregator = "minimal"))
    expect_snapshot(y)

    y$file_info$file_path <- NULL # OS dependent
    y$file_info$`Creation date` <- NULL # OS dependent

    # test mapping
    isotopologs <- tibble(
      compound = "nitrate",
      isotopolog = c("M0", "15N", "17O", "18O"),
      mass = c(61.9878, 62.9850, 62.9922, 63.9922),
      tolerance = 1,
      charge = 1
    )

    expect_snapshot(z <- orbi_identify_isotopocules(y, isotopologs))
    expect_equal(
      z$peaks |> select(-"ions.incremental"),
      orbi_identify_isotopocules(y$peaks, isotopologs)
    ) |>
      suppressMessages()

    # test get
    expect_snapshot(
      out <- y |> orbi_get_data(scans = everything(), spectra = everything())
    )
  })
}) |>
  withr::with_options(new = list(show_exec_times = FALSE))

test_that("orbi_read_raw() and orbi_aggregate_raw() with a status log", {
  # make a copy of an example cache with a (small stand-in) status log, or
  # without any status log (like caches created before isoraw 0.3.1)
  make_cache_with_status_log <- function(dir, status_log = NULL) {
    zip_file <- "nitrate_test_1scan.raw.cache.zip"
    file.copy(
      system.file("extdata", zip_file, package = "isoorbi"),
      file.path(dir, zip_file)
    )
    utils::unzip(file.path(dir, zip_file), exdir = dir)
    cache_dir <- file.path(dir, "nitrate_test_1scan.raw.cache")
    status_log_path <- file.path(cache_dir, "status_log.parquet")
    if (is.null(status_log)) {
      unlink(status_log_path)
    } else {
      arrow::write_parquet(status_log, status_log_path)
    }
    cache_isoraw_output(output_path = cache_dir)
    unlink(cache_dir, recursive = TRUE)
    return(file.path(dir, zip_file))
  }

  # stand-in for the real thing: log entry number, retention time, a section
  # heading and a few channels, all channel values as text (like isoraw)
  status_log <- tibble(
    log.no = 1:3L,
    StartTime = c(0.01, 0.04, 0.07),
    "=====  Temperatures:  =====" = "",
    "Ambient temp. (°C)" = c("30.5394", "30.5401", "30.5533"),
    "Orbitrap block temp. (°C)" = c("33.6771", "33.6772", "33.6774"),
    "ICB: UHV pres. (mbar)" = c("5.41e-011", "5.42e-011", "5.40e-011"),
    .name_repair = "minimal"
  )

  cache_with_log <- make_cache_with_status_log(
    withr::local_tempdir(),
    status_log
  )
  cache_without_log <- make_cache_with_status_log(withr::local_tempdir())

  # reading ====
  x <- orbi_read_raw(
    cache_with_log,
    read_cache = TRUE,
    cache = FALSE,
    show_progress = FALSE
  ) |>
    suppressMessages()
  expect_true("status_log" %in% names(x))
  expect_equal(x$status_log[[1]], status_log)

  # a cache without a status log is still fine, it just doesn't have one
  x_no_log <- orbi_read_raw(
    cache_without_log,
    read_cache = TRUE,
    cache = FALSE,
    show_progress = FALSE
  ) |>
    suppressMessages()
  expect_true("status_log" %in% names(x_no_log))
  expect_equal(x_no_log$status_log[[1]], tibble())

  # aggregating ====
  # none of the included aggregators take anything from the status log, but it
  # is still reported with all of its columns flagged as not aggregated
  y <- orbi_aggregate_raw(x, show_progress = FALSE, show_problems = FALSE) |>
    suppressMessages()
  expect_true("status_log" %in% names(y))
  expect_equal(ncol(y$status_log), 0L)
  expect_equal(attr(y$status_log, "unused_columns"), names(status_log))

  # a file without a status log has nothing to report
  y_no_log <- orbi_aggregate_raw(
    x_no_log,
    show_progress = FALSE,
    show_problems = FALSE
  ) |>
    suppressMessages()
  expect_false("status_log" %in% names(y_no_log))

  # aggregating from the status log works like from any other dataset
  aggregator <- orbi_get_aggregator("standard") |>
    orbi_add_to_aggregator("status_log", "log.no", cast = "as.integer") |>
    orbi_add_to_aggregator(
      "status_log",
      "time.min",
      source = "StartTime",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "status_log",
      "ambientTemperature",
      source = "Ambient temp. (°C)",
      cast = "as.numeric"
    )
  y2 <- orbi_aggregate_raw(
    x,
    aggregator = aggregator,
    show_progress = FALSE,
    show_problems = FALSE
  ) |>
    suppressMessages()
  expect_equal(
    y2$status_log,
    tibble(
      uidx = 1L,
      log.no = 1:3L,
      time.min = c(0.01, 0.04, 0.07),
      ambientTemperature = c(30.5394, 30.5401, 30.5533)
    ),
    # the not aggregated columns are checked separately below
    ignore_attr = "unused_columns"
  )
  expect_equal(
    attr(y2$status_log, "unused_columns"),
    setdiff(names(status_log), c("log.no", "StartTime", "Ambient temp. (°C)"))
  )
  expect_equal(
    y2 |>
      orbi_get_data(status_log = "ambientTemperature") |>
      suppressMessages(),
    tibble(
      uidx = 1L,
      filename = "nitrate_test_1scan",
      ambientTemperature = c(30.5394, 30.5401, 30.5533)
    ),
    ignore_attr = "unused_columns"
  )

  # messages ====
  expect_error(print(y, show_all = "yes"), "must be TRUE or FALSE")
  expect_error(print(y, show_all = NA), "must be TRUE or FALSE")
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot(print(x))
    expect_snapshot(print(y))
    expect_snapshot(print(y2))
    # list all of the not aggregated columns
    expect_snapshot(print(y2, show_all = TRUE))
    # the show all hint only names x if it's a single variable
    expect_snapshot(print(identity(y2)))
    # or the variable in the global environment that holds x (e.g. auto-print)
    assign("agg_data_in_global_env", y2, envir = globalenv())
    withr::defer(rm("agg_data_in_global_env", envir = globalenv()))
    expect_snapshot(y2)
  })
}) |>
  withr::with_options(new = list(show_exec_times = FALSE))
