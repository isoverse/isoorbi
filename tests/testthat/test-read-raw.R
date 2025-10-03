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
  test_that_cli("orbi_read_raw()", configs = c("plain", "fancy"), {
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
  test_that_cli("orbi_read_raw() step2", configs = c("plain", "fancy"), {
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
