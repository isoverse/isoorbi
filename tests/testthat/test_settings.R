# Tests: settings functions to change the default package settings ------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("settings functions")

# orbi_set_settings
test_that("orbi_set_settings() test", {

  # success
  expect_type(orbi_set_settings(di_ref_name = "std"),
               "list")

})

# setting
test_that("setting() test", {

  # failure
  expect_error(setting(),
               "argument \"name\" is missing, with no default", fixed = TRUE)

  expect_error(setting(name = 42),
              "isoorbi setting '42' does not exist", fixed = TRUE)

})
