# Tests: Export functions to export data to a workbook ------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("export functions")

# orbi_export_data_to_excel
test_that("orbi_export_data_to_excel() tests", {

  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  # failure
  expect_error(orbi_export_data_to_excel(),
               "need a `dataset` data frame",
               fixed = TRUE)

  expect_error(orbi_export_data_to_excel(df, 42),
               "`file` needs to be a filepath",
               fixed = TRUE)

  expect_error(orbi_export_data_to_excel(df, "c", "c"),
               "invalid format '%d'; use format %s for character objects",
               fixed = TRUE)

  expect_error(orbi_export_data_to_excel(df, "c", 2, int_format = TRUE),
               "a character vector argument expected",
               fixed = TRUE)

  # success
  tmp_file <- tempfile()
  expect_type(orbi_export_data_to_excel(df, tmp_file, 2, int_format = "0"), "list")
  unlink(tmp_file)
})

# add_excel_sheet
test_that("add_excel_sheet() tests", {

  # failure
  expect_error(add_excel_sheet(),
               "argument \"wb\" is missing, with no default",
               fixed = TRUE)

  expect_error(add_excel_sheet(42),
               "wb must be a Workbok",
               fixed = TRUE)

  library(openxlsx)
  wb <- createWorkbook("test")

  expect_error(add_excel_sheet(wb, "a"),
               "argument \"dataset\" is missing, with no default",
               fixed = TRUE)

  expect_error(add_excel_sheet(wb, c("c,d")),
               "argument \"dataset\" is missing, with no default",
               fixed = TRUE)

  expect_error(add_excel_sheet(wb, "b", 42),
               "no applicable method for 'ungroup' applied to an object of class \"c('double', 'numeric')\"",
               fixed = TRUE)

  expect_error(add_excel_sheet(wb, "c", df, dbl_digits = "a"),
               "no applicable method for 'ungroup' applied to an object of class \"function\"",
               fixed = TRUE)

})
