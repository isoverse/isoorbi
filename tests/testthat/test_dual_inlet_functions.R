# Tests: Functions to annotate and process IsoX data from dual inlet experiments

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("dual inlet functions")

test_that("orbi_dualInlet_define() tests", {

  # safety checks

  expect_error(orbi_dualInlet_define(), "no value for the number of infusion blocks provided",
               fixed = TRUE)
  expect_error(orbi_dualInlet_define(number.of.blocks=1), "no value provided for time needed for new solution to arrive after switching the valve",
               fixed = TRUE)
  expect_error(orbi_dualInlet_define(number.of.blocks=1, switch.time=0), "no value provided for the duration of each infusion block",
               fixed = TRUE)

  expect_error(orbi_dualInlet_define(number.of.blocks="A", block.time=10, switch.time=0), "number.of.blocks needs to be a number",
               fixed = TRUE)

})


test_that("orbi_dualInlet_annotate() tests", {


  # test file

  df.test <- orbi_read_isox(system.file("extdata",
                                   "testfile_dual_inlet.isox",
                                   package = "isoorbi"))

  # annot.test <- orbi_dualInlet_define(
  #   reference = "ref",
  #   sample = "smp",
  #   number.of.blocks = 7,
  #   switch.time = 0,
  #   block.time = 10,
  #   startup.time = 0, #usually 0, time before data run starts
  #   segments = 1 #should segmenting data be a separate small function?
  # )


  # safety checks

  expect_error(orbi_dualInlet_annotate(), "no data provided",
               fixed = TRUE)

  expect_error(orbi_dualInlet_annotate(data = as.character(1:10)),
               "data need to be a R data frame",
               fixed = TRUE)


  expect_error(orbi_dualInlet_annotate(data = df.test),
               "no annotations provided",
               fixed = TRUE)

  expect_error(orbi_dualInlet_annotate(), "no data provided",
               fixed = TRUE)





})
