# functions for logic_isox_file_interactions.R

#' @title Read .isox file
#' @description Read an IsoX output file into a tibble
#'
#' @param filepath Path to the .isox file
#'
#' @return A dataframe containing at minimum named 'filename', 'scan.no', 'time.min', 'compound', 'isotopolog', 'ions.incremental', 'tic', 'it.ms'
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_DualInlet_small.isox", package="isoorbi")
#' df <- iso_read_isox_file(filepath = fpath)
#'
#' @export

iso_read_isox_file <- function(filepath) {
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


