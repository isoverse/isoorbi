# Tests: Functions to annotate and process IsoX data from dual inlet experiments

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("dual inlet functions")

