# functions for interacting with isox data

#' @title Read .isox file
#' @description Read an IsoX output file into a tibble
#'
#' @param filepath Path to the .isox file
#'
#' @return A dataframe containing at minimum named 'filename', 'scan.no', 'time.min', 'compound', 'isotopolog', 'ions.incremental', 'tic', 'it.ms'
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_DualInlet_small.isox", package="isoorbi")
#' df <- orbi_read_isox(filepath = fpath)
#'
#' @export

orbi_read_isox <- function(filepath) {
  # safety checks
  if (missing(filepath)) stop("no file path supplied", call. = FALSE)
  if (length(filepath) != 1) stop("can only read exactly 1 file at the time, supplied paths: ", length(filepath), call. = FALSE)
  if (!file.exists(filepath)) stop("this file does not exist: ", filepath, call. = FALSE)
  ext <- stringr::str_extract(basename(filepath), "\\.[^.]+$")
  if (is.na(ext) || ext != ".isox") stop("unrecognized file extension: ", ext, call. = FALSE)

  tryCatch(
    readr::read_tsv(
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
    ),
    warning = function(w) {
      stop("file format error: ", w$message, call. = FALSE)
    }
  )
}

#' @title Simplify IsoX output
#' @description Keep only columns that are essential for isotopocule ratio analysis
#'
#' @param dataset The loaded IsoX data that is to be simplified
#'
#' @return A simplified data frame with the columns: 'filename', 'scan.no', 'time.min', 'compound', 'isotopolog', 'ions.incremental', 'tic', 'it.ms'.
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_Flow_Exploration_small.isox", package="isoorbi")
#' df <- orbi_read_isox(filepath = fpath) %>% orbi_simplify_isox()
#'
#' @export

orbi_simplify_isox <- function(dataset) {
  df.out <- dataset %>% dplyr::select(.data$filename,
                                      .data$scan.no,
                                      .data$time.min,
                                      .data$compound,
                                      .data$isotopolog,
                                      .data$ions.incremental,
                                      .data$tic,
                                      .data$it.ms)
}


