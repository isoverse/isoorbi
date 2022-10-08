# Functions to load, pre-filter and simplify IsoX data

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
#' fpath <- system.file("extdata", "testfile_dual_inlet.isox", package="isoorbi")
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

  message(paste0("orbi_read_isox() is loading .isox data from file path: \n",
                 filepath))


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
#' @return A tibble containing only the 8 columns: `filename`, `scan.no`, `time.min`, `compound`, `isotopocule`, `ions.incremental`, `tic`, `it.ms`.
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package="isoorbi")
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

  message("orbi_simplify_isox() will keep only the most important columns...")



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


#' @title Basic generic filter for IsoX data
#' @description A basic filter function `orbi_filter_isox()` for file names, isotopocules, compounds and time ranges. Default value for all parameters is FALSE, i.e. no filter is applied.
#' @param dataset The IsoX data to be filtered
#' @param filenames Vector of file names to keep
#' @param compounds Vector of compounds to keep
#' @param isotopocules Vector of isotopocules to keep
#' @param time_min Minimum retention time in minutes (`time.min`)
#' @param time_max Maximum retention time in minutes (`time.min`)
#'
#'@examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
#' df <- orbi_read_isox(filepath = fpath) %>%
#' orbi_simplify_isox() %>%
#' orbi_filter_isox(filenames = c("s3744"),
#' compounds = c("HSO4-"),
#' isotopocules = "M0",
#' time_min = FALSE,
#' time_max = FALSE)
#'
#' @return  Filtered tibble
#' @export
orbi_filter_isox <- function(dataset, filenames = FALSE, compounds = FALSE, isotopocules = FALSE, time_min = FALSE, time_max = FALSE) {


  # safety checks
  if (missing(dataset))
    stop("no dataset supplied", call. = TRUE)
  if (is.data.frame(dataset) == FALSE)
    stop("dataset must be a data frame",  call. = TRUE)
  if (ncol(dataset) < 8)
    stop("dataset must have at least 8 columns: ", ncol(dataset), call. = TRUE)
  if (nrow(dataset) < 1)
    stop("dataset contains no rows: ", nrow(dataset), call. = TRUE)


  if (missing(filenames) && !isFALSE(filenames))
    stop("input for filenames missing", call. = TRUE)
  if (missing(compounds) && !isFALSE(compounds))
    stop("input for compounds missing", call. = TRUE)
  if (missing(isotopocules) && !isFALSE(isotopocules))
    stop("input for isotopocules missing", call. = TRUE)
  if (missing(time_min) && !isFALSE(time_min))
    stop("input for time_min missing", call. = TRUE)
  if (missing(time_max)  && !isFALSE(time_max))
    stop("input for time_max missing", call. = TRUE)


  if (!(is.vector(filenames) | isFALSE(filenames)))
    stop("filenames needs to be a vector of names", call. = TRUE)
  if (!(is.vector(isotopocules) | isFALSE(isotopocules)))
    stop("isotopocules needs to be a vector of names", call. = TRUE)
  if (!(is.vector(compounds) | isFALSE(compounds)))
    stop("compounds needs to be a vector of names", call. = TRUE)

  if (!(is.numeric(time_min) | isFALSE(time_min)))
    stop("time_min needs to be a number", call. = TRUE)
  if (!(is.numeric(time_max) | isFALSE(time_max)))
    stop("time_max needs to be a number", call. = TRUE)


  # check that requires columns are present
  req_cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule", "ions.incremental", "tic", "it.ms")

  missing_cols <- setdiff(req_cols, names(dataset))

  if (length(missing_cols) > 0) {
    paste0("missing required column(s): ", paste(missing_cols, collapse = ", ")) %>%
      stop(call. = FALSE)
  }

  sprintf("orbi_filter_isox() is pre-filtering the data by filename (%s), compounds (%s), isotopocules (%s), minimum (%s) and maximum (%s) time in minutes...",
          paste(filenames, collapse = ", "),
          paste(compounds, collapse = ", "),
          paste(isotopocules, collapse = ", "),
          paste(time_min, collapse = ", "),
          paste(time_max, collapse = ", ")) %>%
    message()

  tryCatch(df.out <- dataset %>%

             # file: filenames
             {if (!isFALSE(filenames))
                 dplyr::filter(., .data$filename %in% filenames)
               else
                 .
             } %>%
             # filter: compounds
             {if (!isFALSE(compounds))
                 dplyr::filter(., .data$compound %in% compounds)
               else
                 .
             } %>%
             # filter: isotopocules
             {if (!isFALSE(isotopocules))
                 dplyr::filter(., .data$isotopocule %in% isotopocules)
               else
                 .
             } %>%
             # filter: time_min
             {if (!isFALSE(time_min))
                 dplyr::filter(., .data$time.min >= time_min)
               else
                 .
             } %>%
             # filter: time_max
             {if (!isFALSE(time_max))
                 dplyr::filter(., .data$time.min <= time_max)
               else
                 .
             },
           warning = function(w) {
             stop("something went wrong: ", w$message, call. = TRUE)
           }
  )

  return(df.out)
}
