# functions for interacting with IsoX data

#' @title Read IsoX file
#' @description Read an IsoX output file (`.isox`) into a tibble data frame
#'
#' @param filepath Path to the `.isox` file
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
#' * `it.ms`: scan injection time (IT) in milli seconds (ms)
#'
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_DualInlet_small.isox", package="isoorbi")
#' df <- orbi_read_isox(filepath = fpath)
#'
#' @return A tibble containing at minimum the columns `filename`, `scan.no`, `time.min`, `compound`, `isotopocule`, `ions.incremental`, `tic`, `it.ms`
#'
#' @export

orbi_read_isox <- function(filepath) {
  # safety checks
  if (missing(filepath)) stop("no file path supplied", call. = TRUE)
  if (length(filepath) != 1) stop("can only read exactly 1 file at the time, supplied paths: ", length(filepath), call. = TRUE)
  if (!file.exists(filepath)) stop("this file does not exist: ", filepath, call. = TRUE)
  ext <- stringr::str_extract(basename(filepath), "\\.[^.]+$")
  if (is.na(ext) || ext != ".isox") stop("unrecognized file extension: ", ext, call. = TRUE)

  tryCatch(

    df <- readr::read_tsv(
      filepath,
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
    ) %>% dplyr::rename(isotopocule = .data$isotopolog), # isox format should eventually change as well
    warning = function(w) {
      stop("file format error: ", w$message, call. = TRUE)
    }
  )

  # check that all the most important columns are present
  req_cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule", "ions.incremental", "tic", "it.ms")

  missing_cols <- setdiff(req_cols, names(df))

  if (length(missing_cols) > 0) {
    paste0("Missing required column(s): ", paste(missing_cols, collapse = ", ")) %>%
      stop(call. = FALSE)
  }

  return(df)

}

#' @title Simplify IsoX data
#' @description Keep only columns that are directly relevant for isotopocule ratio analysis
#'
#' @param dataset IsoX data that is to be simplified
#'
#' @return A tiblle containing only the 8 columns: `filename`, `scan.no`, `time.min`, `compound`, `isotopocule`, `ions.incremental`, `tic`, `it.ms`.
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_Flow_Exploration_small.isox", package="isoorbi")
#' df <- orbi_read_isox(filepath = fpath) %>% orbi_simplify_isox()
#'
#' @export

orbi_simplify_isox <- function(dataset) {
  # safety checks
  if (missing(dataset))
    stop("no dataset supplied", call. = TRUE)
  if (is.data.frame(dataset) == FALSE)
    stop("dataset must be a data frame",  call. = TRUE)
  if (ncol(dataset) < 8)
    stop("dataset must have at least 8 columns: ", ncol(dataset), call. = TRUE)
  if (nrow(dataset) < 1)
    stop("dataset contains no rows: ", nrow(dataset), call. = TRUE)

   # check that requires columns are present
  req_cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule", "ions.incremental", "tic", "it.ms")

  missing_cols <- setdiff(req_cols, names(dataset))

  if (length(missing_cols) > 0) {
    paste0("Missing required column(s): ", paste(missing_cols, collapse = ", ")) %>%
      stop(call. = FALSE)
  }


  tryCatch(
    dataset %>% dplyr::select(
      .data$filename,
      .data$compound,
      .data$scan.no,
      .data$time.min,
      .data$isotopocule,
      .data$ions.incremental,
      .data$tic,
      .data$it.ms
    ),
    warning = function(w) {
      stop("format error: ", w$message, call. = FALSE)
    }
  )
}


