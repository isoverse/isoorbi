# Tests: Functions to load, pre-filter and simplify IsoX data

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("loading functions")

test_that("orbi_find_isox", {

  # safety checks
  expect_error(orbi_find_isox(), "argument \"folder\" is missing, with no default", fixed = TRUE)
  expect_error(orbi_find_isox(42), "invalid filename argument")
  expect_error(orbi_find_isox("DNE"), "folder` must be an existing directory", fixed = TRUE)

})

test_that("orbi_read_isox() tests", {

  # safety checks
  expect_error(orbi_read_isox(), "no file path supplied", fixed = TRUE)
  expect_error(orbi_read_isox(42), "`file` has to be at least one filepath")
  expect_error(orbi_read_isox(character()), "`file` has to be at least one filepath")
  expect_error(orbi_read_isox("DNE"), "does not exist", fixed = TRUE)

  temp_file <- tempfile(fileext = ".wrong")

  cat("empty", file = temp_file) # create the temp file

  expect_error(orbi_read_isox(temp_file), "unrecognized file extension", fixed = TRUE)

  unlink(temp_file) # destroy the temp file

  # corrupt files - missing columns
  expect_error(orbi_read_isox(file.path(base_dir, "test_files", "missing_column.isox")), "file format error")
  # corrupt files
  expect_error(orbi_read_isox(file.path(
    base_dir, "test_files", "missing_column.isox"
  )), "file format error",
  fixed = TRUE)

  # test reading a file
  expect_true(
    is.tbl(df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")))
  )

  expect_equal(
    names(df),
    c(
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

  expect_equal(nrow(df), 5184)

  # test reading multiple files
  expect_true(
    is.tbl(
      df2 <-
        orbi_read_isox(c(
          system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"),
          system.file("extdata", "testfile_flow.isox", package = "isoorbi")
        ))
    )
  )
  expect_equal(nrow(df2), 11633)

})

# orbi_simplify_isox
test_that("orbi_simplify_isox() tests", {

  # success
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(orbi_simplify_isox(df)))

  # test safety checks
  expect_error(orbi_simplify_isox(), "need a `dataset` data frame")
  expect_error(orbi_simplify_isox(dataset = "string"), "need a `dataset` data frame")
  expect_type(df, "list")
  dataset = subset(df, select = -c(filename))
  expect_error(orbi_simplify_isox(dataset), "dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`")
  dataset = df[0,]
  expect_error(orbi_simplify_isox(dataset), "dataset contains no rows")
  # failure

  expect_error(orbi_simplify_isox(), "need a `dataset` data frame",
               fixed = TRUE)

  expect_error(orbi_simplify_isox(dataset = as.matrix(df)), "need a `dataset` data frame", fixed = TRUE)

  expect_error(orbi_simplify_isox(dataset = df[,1:5]),
               "dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`")
  dataset = df[0,]
  expect_error(orbi_simplify_isox(dataset),
               "dataset contains no rows")

  df2 <- df |> mutate(dummy = "1") |> select(-scan.no)
  expect_error(orbi_simplify_isox(dataset = df2),
               "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`", fixed = TRUE)

})

# orbi_filter_isox()

test_that("orbi_filter_isox() tests",{

  # success
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(orbi_filter_isox(df)))

  expect_true(is.tbl(orbi_filter_isox(df,
                                      filenames = "20220125_01",
                                      compounds = "NO3-",
                                      isotopocules = c("15N", "18O"),
                                      time_min = 0.5,
                                      time_max = 2)))

  # failure
  expect_error(orbi_filter_isox(), "need a `dataset` data frame",
               fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = as.matrix(df)),
               "need a `dataset` data frame",
               fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = df[,1:2]),
               "`dataset` requires columns `filename`, `compound`, `scan.no`, `tic` and `it.ms`")

  expect_error(orbi_filter_isox(dataset = df,
                                time_min = "A"),
               "`time_min` must be a single number (or NULL)", fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = df,
                                time_max = "A"),
               "`time_max` must be a single number (or NULL)", fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = df,
                                time_min = c(0.1, 0.2)),
               "`time_min` must be a single number (or NULL)", fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = df,
                                time_max = c(0.1, 0.2)),
               "`time_max` must be a single number (or NULL)", fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = df,
                                filenames = as.matrix(c(1, 0))),
               "`filenames` must be a vector of filenames (or NULL)", fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = df,
                                isotopocules = as.matrix(c(1, 0))),
               "`isotopocules` must be a vector of isotopocules (or NULL)", fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = df,
                                compounds = as.matrix(c(1, 0))),
               "`compounds` must be a vector of compounds (or NULL)", fixed = TRUE)

})
