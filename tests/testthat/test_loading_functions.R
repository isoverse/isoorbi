# Tests: Functions to load, pre-filter and simplify IsoX data

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("isox data")

test_that("test that isox files can be read", {

  # test safety checks
  expect_error(orbi_read_isox(c("one", "two")), "can only read.*1")
  expect_error(orbi_read_isox("DNE"), "does not exist")
  temp_file <- tempfile(fileext = ".wrong")
  cat("empty", file = temp_file) # create the temp file
  expect_error(orbi_read_isox(temp_file), "unrecognized")
  unlink(temp_file) # destroy the temp file

  # corrupt files
  expect_error(orbi_read_isox(file.path(base_dir, "test_files", "missing_column.isox")), "file format error")

  # test reading a file
  expect_true(is.tbl(df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))))
  expect_equal(names(df), c("filename", "scan.no", "time.min", "compound", "isotopocule", "ions.incremental", "tic", "it.ms"))
  expect_equal(nrow(df), 5184)

})

# orbi_filter_isox
test_that("orbi_filter_isox() tests",{
  #success
  #failure
  expect_error(orbi_filter_isox(), "no dataset supplied")
})

# orbi_simplify_isox
test_that("orbi_simplify_isox() tests", {
  #success
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))
  expect_true(is.tbl(orbi_simplify_isox(df)))

  # test safety checks
  expect_error(orbi_simplify_isox(), "no dataset supplied")
  #add 10 more tests
})


