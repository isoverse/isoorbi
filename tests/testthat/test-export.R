# Tests: Export functions to export data to a workbook ------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

# add_excel_sheet
test_that("test add_excel_sheet()", {
  library(openxlsx)
  wb <- createWorkbook("test")

  # failures
  add_excel_sheet() |> expect_error("wb.*must be.*workbook")
  add_excel_sheet(wb) |> expect_error("sheet_name.*must be a string")
  add_excel_sheet(wb, "a") |> expect_error("dataset.*must be a data frame")
  add_excel_sheet(wb, "a", 42) |> expect_error("dataset.*must be a data frame")
  add_excel_sheet(wb, "a", mtcars, dbl_digits = "a") |>
    expect_error("dbl_digits.*must be an integer")
  add_excel_sheet(wb, "a", mtcars, int_format = TRUE) |>
    expect_error("int_format.*must be a string")
  add_excel_sheet(wb, "a", mtcars, dbl_format = TRUE) |>
    expect_error("dbl_format.*must be a string")
})

# orbi_export_data_to_excel
test_that("test orbi_export_data_to_excel()", {
  # failure
  orbi_export_data_to_excel() |> expect_error("dataset.*must be a data frame")
  orbi_export_data_to_excel(mtcars, 42) |>
    expect_error("file.*must be a filepath")

  # success
  tmp_file <- tempfile()
  orbi_export_data_to_excel(mtcars, tmp_file, 2, int_format = "0") |>
    expect_message("Exporting.*dataset.*32 rows.*11 columns") |>
    suppressMessages()
  unlink(tmp_file)
})
