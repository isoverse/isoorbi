#' @title Export data to excel
#' @description This functions exports the `dataset` into an Excel file. If the `dataset` is aggregated data, use the `include` parameter to decide which part of the data to export.
#' @inheritParams orbi_flag_satellite_peaks
#' @inheritParams orbi_aggregate_raw
#' @param file file path to export the file - recursively creates the directory if non-existent
#' @param dbl_digits how many digits to show for dbls (all are exported)
#' @param int_format the excel formatting style for integers
#' @param dbl_format the excel formatting style for doubles (created automatically from the dbl_digits parameter)
#' @param include which tibbles to include if `dataset` is aggregated data. By default includes all but spectra
#' @export
#' @return returns dataset invisibly for use in pipes
orbi_export_data_to_excel <- function(
  dataset,
  file,
  dbl_digits = 7,
  int_format = "0",
  dbl_format = sprintf(sprintf("%%.%sf", dbl_digits), 0),
  include = c("file_info", "summary", "scans", "peaks", "problems"),
  show_progress = rlang::is_interactive()
) {
  # check for availability
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warn(
      "The 'openxlsx' package is required to export to this file format, please install it using the following command: install.packages(\"openxlsx\")"
    )
    return(invisible(dataset))
  }

  # safety checks
  check_dataset_arg(dataset)
  check_arg(
    file,
    !missing(file) && is_scalar_character(file),
    "must be a filepath"
  )
  check_arg(include, is_character(include), "must be a character vector")

  # Check if output path exists
  output_dir <- dirname(file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # info
  start <- start_info("is writing {.field {pb_status}} | {pb_elapsed}")

  # make excel workbook
  wb <- openxlsx::createWorkbook()

  # add excel sheet
  if (is(dataset, "orbi_aggregated_data")) {
    # aggregated dataset
    sheets <- intersect(include, names(dataset))
    if (length(sheets) == 0) {
      cli_abort(
        "none of the requested tables ({.field {include}}) are available in the {.field dataset}"
      )
    }
    info <- c()
    for (sheet in sheets) {
      # progress
      if (show_progress) {
        cli_progress_update(
          id = start$pb,
          inc = 0,
          status = sheet,
          force = TRUE
        )
      }
      # info
      info <- c(
        info,
        format_inline(
          "{format_number(nrow(dataset[[sheet]]))}{qty(nrow(dataset[[sheet]]))} row{?s} of {.field {sheet}}"
        )
      )
      # export
      add_excel_sheet(
        wb,
        sheet_name = sheet,
        # always include filename
        dataset = if (!"filename" %in% names(dataset[[sheet]])) {
          dataset[[sheet]] |>
            dplyr::left_join(
              dataset$file_info[c("uidx", "filename")],
              by = "uidx"
            ) |>
            dplyr::relocate("filename", .after = "uidx")
        } else {
          dataset[[sheet]]
        },
        dbl_digits = dbl_digits,
        int_format = int_format,
        dbl_format = dbl_format
      )
    }
  } else {
    # single data
    info <- format_inline(
      "{format_number(nrow(dataset))}{qty(nrow(dataset))} row{?s}, {ncol(dataset)} column{?s}"
    )
    add_excel_sheet(
      wb,
      sheet_name = "dataset",
      dataset = dataset,
      dbl_digits = dbl_digits,
      int_format = int_format,
      dbl_format = dbl_format
    )
  }

  # save workbook
  if (show_progress) {
    cli_progress_update(id = start$pb, inc = 0, status = "file", force = TRUE)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  # info
  finish_info(
    "exported the {.field dataset} ({info}) to {.file {file}}",
    start = start
  )

  # return invisible
  return(invisible(dataset))
}


# @title Add a new sheet
# @description Internal function to add an excel sheet to a workbook.
# @param wb the workbook object
# @param sheet_name the name of the excel sheet
# @param dataset the data frame(s)
# @param dbl_digits how many digits to show for dbls (all are exported)
# @param int_format the excel formatting style for integers
# @param dbl_format the excel formatting style for doubles (created automatically from the dbl_digits parameter)
# @param col_max_width maximum column width
add_excel_sheet <- function(
  wb,
  sheet_name,
  dataset,
  dbl_digits = 2,
  col_max_width = 75,
  int_format = "0",
  dbl_format = sprintf(sprintf("%%.%sf", dbl_digits), 0)
) {
  # safety checks
  check_arg(
    wb,
    !missing(wb) && is(wb, "Workbook"),
    "must be an openxlsx workbook"
  )
  check_arg(
    sheet_name,
    !missing(sheet_name) && is_scalar_character(sheet_name),
    "must be a string"
  )
  check_arg(
    dataset,
    !missing(dataset) && is.data.frame(dataset),
    "must be a data frame or tibble"
  )
  check_arg(dbl_digits, is_scalar_integerish(dbl_digits), "must be an integer")
  check_arg(
    col_max_width,
    is_scalar_integerish(col_max_width),
    "must be an integer"
  )
  check_arg(int_format, is_scalar_character(int_format), "must be a string")
  check_arg(dbl_format, is_scalar_character(dbl_format), "must be a string")

  # sheet
  openxlsx::addWorksheet(wb, sheet_name)
  hs <- openxlsx::createStyle(textDecoration = "bold") # header style

  # data
  start_row <- 1L
  sheet_data <- dplyr::ungroup(dataset)
  if (ncol(sheet_data) > 0) {
    # write data
    openxlsx::writeData(
      wb,
      sheet_name,
      sheet_data,
      startRow = start_row,
      headerStyle = hs
    )
    int_cols <- which(purrr::map_lgl(sheet_data, is.integer))
    dbl_cols <- setdiff(which(purrr::map_lgl(sheet_data, is.numeric)), int_cols)
    if (dbl_digits < 1) {
      int_cols <- c(int_cols, dbl_cols)
      dbl_cols <- integer()
    }

    # integer column formatting
    if (length(int_cols) > 0) {
      openxlsx::addStyle(
        wb,
        sheet_name,
        style = openxlsx::createStyle(numFmt = int_format),
        rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
        cols = int_cols,
        gridExpand = TRUE
      )
    }

    # double column formatting
    if (length(dbl_cols) > 0) {
      openxlsx::addStyle(
        wb,
        sheet_name,
        style = openxlsx::createStyle(numFmt = dbl_format),
        rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
        cols = dbl_cols,
        gridExpand = TRUE
      )
    }

    # calculate header widths
    header_widths <- sheet_data |> names() |> map_int(nchar)

    # calculate data widths
    calculate_data_width <- function(x) {
      if (is.integer(x)) {
        x <- sprintf("%d", x)
      } else if (is.numeric(x)) {
        x <- sprintf(paste0("%.", dbl_digits, "f"), x)
      } else {
        x <- as.character(x)
      }
      return(max(c(0, nchar(x)), na.rm = TRUE))
    }

    # column max widths
    data_widths <-
      sheet_data |>
      dplyr::summarize(across(dplyr::everything(), calculate_data_width)) |>
      unlist(use.names = FALSE)
    col_widths <- pmax(header_widths, data_widths)
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = 1:length(col_widths),
      widths = col_widths
    )
  }
}
