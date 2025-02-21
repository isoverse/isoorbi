# Tests: Functions to load, pre-filter and simplify IsoX data

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

test_that("orbi_find_raw() works", {

  # safety checks
  expect_error(orbi_find_raw(), "argument \"folder\" is missing, with no default", fixed = TRUE)
  expect_error(orbi_find_raw(42), "invalid filename argument")
  expect_error(orbi_find_raw("DNE"), "folder` must be an existing directory", fixed = TRUE)

})

test_that("orbi_read_raw() works", {

  # safety checks
  expect_error(orbi_read_raw(), "no file path supplied", fixed = TRUE)
  expect_error(orbi_read_raw(42), "`file_paths` has to be at least one file path") 
  expect_error(orbi_read_raw(character()), "`file_paths` has to be at least one file path")
  expect_message(orbi_read_raw("DNE"), "encountered.*1.*error") |>
    expect_message("does not exist")

})
