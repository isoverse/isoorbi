# Tests: Functions to annotate and process IsoX data from dual inlet experiments

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("dual inlet functions")

test_that("orbi_dualInlet_define() tests", {

  # safety checks

  expect_error(orbi_dualInlet_define(), "no value for the number of infusion blocks provided",
               fixed = TRUE)

})


test_that("orbi_dualInlet_annotate() tests", {

  # safety checks

  expect_error(orbi_dualInlet_annotate(), "no data provided",
               fixed = TRUE)

})
