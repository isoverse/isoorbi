#' export data frame to excel
#' @param dataset data frame
#' @param file file path to export the file
#' @inheritParams add_excel_sheet
#' @export
#' @return returns dataset invisibly for use in pipes
#'
orbi_export_data_to_excel <- function(dataset, file, dbl_digits = 2, int_format = "0", dbl_format = sprintf(sprintf("%%.%df", dbl_digits), 0)) {

  # check for availability
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warn(
      "The 'openxlsx' package is required to export to this file format, please install it using the following command: install.packages(\"openxlsx\")"
    )
    return(invisible(dataset))
  }

  # safety checks
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`file` needs to be a filepath" = !missing(file) && is_scalar_character(file)
  )

  # info
  start_time <-
    sprintf(
      "orbi_export_data_to_excel() is exporting data set with %d rows and %d columns to %s... ",
      nrow(dataset), ncol(dataset), file
    ) |>
    message_start()

  # make excel workbook
  wb <- openxlsx::createWorkbook()

  # add excel sheet
  add_excel_sheet(
    wb,
    sheet_name = "dataset",
    dataset = dataset,
    dbl_digits = dbl_digits,
    int_format = int_format,
    dbl_format = dbl_format
  )

  # save workbook
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  # info
  message_finish("completed", start_time = start_time)

  # return invisible
  return(invisible(dataset))
}


#' internal function to add an excel sheet to a workbook
#' @param wb the workbook object
#' @param sheet_name the name of the excel sheet
#' @param dataset the data frame(s)
#' @param dbl_digits how many digits to show for dbls (all are exported)
#' @param int_format the excel formatting style for integers
#' @param dbl_format the excel formatting style for doubles (created automatically from the dbl_digits parameter)
#' @param col_max_width maximum column width
add_excel_sheet <- function(wb, sheet_name, dataset, dbl_digits = 2, col_max_width = 75, int_format = "0", dbl_format = sprintf(sprintf("%%.%df", dbl_digits), 0)) {

  # sheet
  openxlsx::addWorksheet(wb, sheet_name)
  hs <- openxlsx::createStyle(textDecoration = "bold") # header style

  # data
  start_row <- 1L
  sheet_data <- dplyr::ungroup(dataset)
  if (ncol(sheet_data) > 0) {

    # write data
    openxlsx::writeData(wb, sheet_name, sheet_data, startRow = start_row, headerStyle = hs)
    int_cols <- which(purrr::map_lgl(sheet_data, is.integer))
    dbl_cols <- setdiff(which(purrr::map_lgl(sheet_data, is.numeric)), int_cols)
    if (dbl_digits < 1) {
      int_cols <- c(int_cols, dbl_cols)
      dbl_cols <- integer()
    }

    # integer column formatting
    if (length(int_cols) > 0) {
      openxlsx::addStyle(
        wb, sheet_name, style = openxlsx::createStyle(numFmt = int_format),
        rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
        cols = int_cols, gridExpand = TRUE)
    }

    # double column formatting
    if (length(dbl_cols) > 0) {
      openxlsx::addStyle(
        wb, sheet_name, style = openxlsx::createStyle(numFmt = dbl_format),
        rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
        cols = dbl_cols, gridExpand = TRUE)
    }

    # calculate header widths
    header_widths <- sheet_data |> names() |> map_int(nchar)

    # calculate data widths
    calculate_data_width <- function(x) {
      if (is.integer(x)) x <- sprintf("%d", x)
      else if (is.numeric(x)) x <- sprintf(paste0("%.", dbl_digits, "f"), x)
      else x <- as.character(x)
      return(max(c(0, nchar(x)), na.rm = TRUE))
    }

    # column max widths
    data_widths <-
      sheet_data |>
      dplyr::summarize(across(dplyr::everything(), calculate_data_width)) |>
      unlist(use.names = FALSE)
    col_widths <- pmax(header_widths, data_widths)
    openxlsx::setColWidths(wb, sheet_name, cols = 1:length(col_widths), widths = col_widths)
  }

}
