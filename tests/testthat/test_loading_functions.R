# Tests: Functions to load, pre-filter and simplify IsoX data

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("loading functions")

test_that("orbi_read_isox() tests", {


  # safety checks

  expect_error(orbi_read_isox(), "no file path supplied",
               fixed = TRUE)

  expect_error(orbi_read_isox(c("one", "two")), "can only read.*1")

  expect_error(orbi_read_isox("DNE"), "does not exist",
               fixed = TRUE)

  temp_file <- tempfile(fileext = ".wrong")

  cat("empty", file = temp_file) # create the temp file

  expect_error(orbi_read_isox(temp_file), "unrecognized",
               fixed = TRUE)

  unlink(temp_file) # destroy the temp file

  # corrupt files
  expect_error(orbi_read_isox(file.path(
    base_dir, "test_files", "missing_column.isox"
  )), "file format error",
  fixed = TRUE)

  # test reading a file

  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(df))

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


})


# orbi_simplify_isox
test_that("orbi_simplify_isox() tests", {

  # success
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(orbi_simplify_isox(df)))

})



# orbi_filter_isox()

test_that("orbi_filter_isox() tests",{

  # failure
  expect_error(orbi_filter_isox(), "no dataset supplied",
               fixed = TRUE)

  expect_error(orbi_simplify_isox(), "no dataset supplied",
               fixed = TRUE)

  # success
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(orbi_filter_isox(df)))

})
