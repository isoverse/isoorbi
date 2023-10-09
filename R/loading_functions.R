# Functions to load, pre-filter and simplify IsoX data ------------------------------------

#' Find isox files
#'
#' Finds all .isox files in a folder.
#'
#' @param folder path to a folder with isox files
#' @param recursive whether to find files recursively
#'
#' @examples
#'
#' # all .isox files provided with the isoorbi package
#' orbi_find_isox(system.file("extdata", package = "isoorbi"))
#'
#' @export
orbi_find_isox <- function(folder, recursive = TRUE) {
  # safety check
  stopifnot(
    "`folder` must be an existing directory" = dir.exists(folder),
    "`recursive` must a boolean" = is_scalar_logical(recursive)
  )

  # return
  list.files(folder, pattern = "\\.isox", full.names = TRUE, recursive = recursive)
}

#' @title Read IsoX file
#' @description Read an IsoX output file (`.isox`) into a tibble data frame
#'
#' @param file Path to the `.isox` file(s), single value or vector of paths
#' @details Additional information on the columns:
#'
#' * `filename`: name of the original Thermo `.raw` file processed by IsoX
#'
#' * `scan.no`: scan number
#'
#' * `time.min`: acquisition or retention time in minutes
#'
#' * `compound`: name of the compound (e.g., NO3-)
#'
#' * `isotopocule`: name of the isotopocule (e.g., 15N); called `isotopolog` in `.isox`
#'
#' * `ions.incremental`: estimated number of ions, in increments since it is a calculated number
#'
#' * `tic`: total ion current (TIC) of the scan
#'
#' * `it.ms`: scan injection time (IT) in millisecond (ms)
#'
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")
#' df <- orbi_read_isox(file = fpath)
#'
#' @return A tibble containing at minimum the columns `filename`, `scan.no`, `time.min`, `compound`, `isotopocule`, `ions.incremental`, `tic`, `it.ms`
#'
#' @export
orbi_read_isox <- function(file) {

  # safety checks
  stopifnot(
    "no file path supplied" = !missing(file),
    "`file` has to be at least one filepath" = is_character(file) && length(file) >= 1L
  )

  if (any(missing <- !file.exists(file)))
    sprintf("file does not exist: '%s'", paste(file[missing], collapse = "', '")) |>
    abort()

  # ext <- stringr::str_extract(basename(file), "\\.[^.]+$")
  # use base r to skip stringr dependency (since it's not used elsewhere)
  ext <- sub("^.+\\.([^.]+)$", "\\1", basename(file))
  if (any(ext != "isox"))
    abort("unrecognized file extension, only .isox is supported")

  # info
  sprintf("orbi_read_isox() is loading .isox data from %d file(s)...", length(file)) |>
    message_standalone()

  # read files
  df <-
    try_catch_all(
      file |>
        map(
          ~{
            start_time <- Sys.time()
            data <- readr::read_tsv(
              .x,
              col_types = list(
                filename = readr::col_factor(),
                scan.no = readr::col_integer(),
                time.min = readr::col_double(),
                compound = readr::col_factor(),
                isotopolog = readr::col_factor(),
                ions.incremental = readr::col_double(),
                tic = readr::col_double(),
                it.ms = readr::col_double()
              )
            )
            end_time <- Sys.time()
            sprintf(" - loaded %d peaks for %d compounds (%s) with %d isotopocules (%s) from %s",
                    nrow(data),
                    length(levels(data$compound)),
                    paste(levels(data$compound), collapse = ", "),
                    length(levels(data$isotopolog)),
                    paste(levels(data$isotopolog), collapse = ", "),
                    basename(.x)) |>
              message_standalone(start_time = start_time)
            data
          }
        ) |>
        dplyr::bind_rows() |>
        # .isox format should eventually change as well to `isotopocule`
        dplyr::rename(isotopocule = "isotopolog"),
      "file format error: ",
      newline = FALSE
  )

  # check that all the most important columns are present
  req_cols <-
    c(
      "filename",
      "compound",
      "scan.no",
      "time.min",
      "isotopocule",
      "ions.incremental",
      "tic",
      "it.ms"
    )

  missing_cols <- setdiff(req_cols, names(df))

  if (length(missing_cols) > 0) {
    paste0("Missing required column(s): ",
           paste(missing_cols, collapse = ", ")) |>
      abort()
  }

  return(df)

}

#' @title Simplify IsoX data
#' @description Keep only columns that are directly relevant for isotopocule ratio analysis
#'
#' @param dataset IsoX data that is to be simplified
#'
#' @return A tibble containing only the 8 columns: `filename`, `scan.no`, `time.min`, `compound`, `isotopocule`, `ions.incremental`, `tic`, `it.ms`.
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package="isoorbi")
#' df <- orbi_read_isox(file = fpath) |> orbi_simplify_isox()
#'
#' @export

orbi_simplify_isox <- function(dataset) {

  # safety checks
  cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule", "ions.incremental", "tic", "it.ms")
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`" =
      all(cols %in% names(dataset))
  )
  if (nrow(dataset) < 1)
    stop("dataset contains no rows", call. = TRUE)

  # info
  start_time <-
    sprintf("orbi_simplify_isox() will keep only columns '%s'... ",
            paste(cols, collapse = "', '")) |>
    message_start()

  # select
  dataset_out <- try_catch_all(
    dataset |> dplyr::select(!!!cols),
    "format error: "
  )

  # info
  sprintf("complete") |> message_finish(start_time = start_time)

  # return
  return(dataset_out)
}

#' @title Basic generic filter for IsoX data
#' @description A basic filter function `orbi_filter_isox()` for file names, isotopocules, compounds and time ranges. Default value for all parameters is NULL, i.e. no filter is applied.
#'
#' @param dataset The IsoX data to be filtered
#' @param filenames Vector of file names to keep, keeps all if set to `NULL` (the default)
#' @param compounds Vector of compounds to keep, keeps all if set to `NULL` (the default)
#' @param isotopocules Vector of isotopocules to keep, keeps all if set to `NULL` (the default)
#' @param time_min Minimum retention time in minutes (`time.min`), no minimum if set to `NULL` (the default)
#' @param time_max Maximum retention time in minutes (`time.min`), no maximum if set to `NULL` (the default)
#'
#'@examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
#' df <-
#'   orbi_read_isox(file = fpath) |>
#'   orbi_simplify_isox() |>
#'   orbi_filter_isox(
#'     filenames = c("s3744"),
#'     compounds = "HSO4-",
#'     isotopocules = c("M0", "34S", "18O")
#'   )
#'
#' @return Filtered tibble
#' @export
orbi_filter_isox <-
  function(dataset,
           filenames = NULL,
           compounds = NULL,
           isotopocules = NULL,
           time_min = NULL,
           time_max = NULL) {

  # safety checks
  cols <- c("filename", "compound", "isotopocule", "time.min")
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `tic` and `it.ms`" =
      all(cols %in% names(dataset)),
    "`filenames` must be a vector of filenames (or NULL)" = is.null(filenames) || is_character(filenames),
    "`compounds` must be a vector of compounds (or NULL)" = is.null(compounds) || is_character(compounds),
    "`isotopocules` must be a vector of isotopocules (or NULL)" = is.null(isotopocules) || is_character(isotopocules),
    "`time_min` must be a single number (or NULL)" = is.null(time_min) || is_scalar_double(time_min),
    "`time_max` must be a single number (or NULL)" = is.null(time_max) || is_scalar_double(time_max)
  )

  # info
  filters <- c()
  if(!is.null(filenames))
    filters <- c(filters, sprintf("filenames (%s)", paste(filenames, collapse = ", ")))
  if(!is.null(compounds))
    filters <- c(filters, sprintf("compounds (%s)", paste(compounds, collapse = ", ")))
  if(!is.null(isotopocules))
    filters <- c(filters, sprintf("isotopocules (%s)", paste(isotopocules, collapse = ", ")))
  if(!is.null(time_min))
    filters <- c(filters, sprintf("minimum time (%s minutes)", time_min))
  if(!is.null(time_max))
    filters <- c(filters, sprintf("maximum time (%s minutes)", time_min))

  # info
  n_row_start <- nrow(dataset)
  start_time <-
    sprintf(
      "orbi_filter_isox() is filtering the dataset by %s... ", paste(filters, collapse = ", ")
    ) |>
    message_start()

  # filtering
  dataset <-
    try_catch_all({
      # file: filenames
      if (!is.null(filenames))
        dataset <- dataset |> dplyr::filter(.data$filename %in% !!filenames)

      # filter: compounds
      if (!is.null(compounds))
        dataset <- dataset |> dplyr::filter(.data$compound %in% !!compounds)

      # filter: isotopocules
      if (!is.null(isotopocules))
        dataset <- dataset |> dplyr::filter(.data$isotopocule %in% !!isotopocules)

      # filter: time_min
      if (!is.null(time_min))
        dataset <- dataset |> dplyr::filter(.data$time.min >= !!time_min)

      # filter: time_max
      if (!is.null(time_max))
        dataset <- dataset |> dplyr::filter(.data$time.min <= !!time_max)

      # return
      dataset
    },
    "something went wrong trying to filter dataset: "
  )

  # info
  sprintf(
    "removed %d/%d rows (%.1f%%)",
    n_row_start - nrow(dataset), n_row_start, (n_row_start - nrow(dataset))/n_row_start * 100) |>
    message_finish(start_time = start_time)

  # return
  return(dataset |> droplevels())
}
