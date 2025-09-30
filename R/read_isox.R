# Functions to load, pre-filter and simplify IsoX data ------------------------------------

#' @title Find isox files
#' @description Finds all .isox files in a folder.
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
  check_arg(
    folder,
    !missing(folder) &&
      is_character(folder) &&
      length(folder) > 0 &&
      all(dir.exists(folder)),
    format_inline(
      "must point to {qty(if(!missing(folder)) length(folder) else 1)}{?an/} existing director{?y/ies}"
    ),
    include_type = FALSE,
    include_value = TRUE
  )
  check_arg(recursive, is_scalar_logical(recursive), "must be TRUE or FALSE")

  # return
  list.files(
    folder,
    pattern = "\\.isox",
    full.names = TRUE,
    recursive = recursive
  )
}

#' @title Read IsoX file
#' @description Read an IsoX dataput file (`.isox`) into a tibble data frame.
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

  # file paths provided?
  check_arg(
    file,
    !missing(file) && is_character(file) && length(file) > 0,
    format_inline("must be a file path")
  )

  # any files doen't exist
  missing <- !file.exists(file)
  check_arg(
    file,
    !any(missing),
    format_inline(
      "points to {?a/} file{?s} that do{?es/} not exist: {.file {file[missing]}}"
    ),
    include_type = FALSE,
    include_value = FALSE
  )

  # use base r to skip stringr dependency (since it's not used elsewhere)
  ext <- sub("^.+\\.([^.]+)$", "\\1", basename(file))
  if (any(ext != "isox")) {
    cli_abort(
      "unrecognized file extension {.field .{ext}}, only {.field .isox} is supported"
    )
  }

  # info
  start <- start_info("is reading .isox data from {length(file)} file{?s}")

  # read files
  df <-
    dplyr::tibble(filepath = file) |>
    dplyr::mutate(
      out = map(
        .data$filepath,
        ~ {
          start_i <- start_info()
          out <- readr::read_tsv(
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
          ) |>
            try_catch_cnds()
          # individual file info
          finish_info(
            "loaded {nrow(out$result)} peak{?s}",
            if ("compound" %in% names(out$result)) {
              " for {length(levels(out$result$compound))} compound{?s} ({.field {levels(out$result$compound)}})"
            },
            if ("isotopolog" %in% names(out$result)) {
              " with {length(levels(out$result$isotopolog))} isotopocule{?s} ({.field {levels(out$result$isotopolog)}})"
            },
            " from {.file {basename(.x)}}",
            start = start_i,
            conditions = out$conditions,
            .call = expr(orbi_read_isox())
          )
          out
        }
      )
    )

  # encountered issues
  conditions <- purrr::map(df$out, ~ .x$conditions) |> dplyr::bind_rows()

  # unnest results
  df <- df |>
    dplyr::mutate(result = purrr::map(.data$out, ~ .x$result)) |>
    dplyr::select(-"out") |>
    tidyr::unnest("result") |>
    # .isox format should eventually change as well to `isotopocule`
    dplyr::rename(dplyr::any_of(c(isotopocule = "isotopolog")))

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

  # check for missing columns
  missing_cols <- setdiff(req_cols, names(df))
  if (length(missing_cols) > 0) {
    cli_abort("missing required column{?s}: {.field {missing_cols}}")
  }

  # info
  if (length(file) > 1 || nrow(conditions) > 0) {
    finish_info(
      "read {.file .isox} data from {length(file)} file{?s}",
      start = start,
      conditions = conditions,
      show_conditions = FALSE
    )
  }

  return(df)
}

#' @title Simplify IsoX data
#' @description Keep only columns that are directly relevant for isotopocule ratio analysis. This function is optional and does not affect any downstream function calls.
#'
#' @param dataset IsoX data that is to be simplified
#' @param add additional columns to keep
#'
#' @return A tibble containing only the 9 columns: `filepath`, `filename`, `scan.no`, `time.min`, `compound`, `isotopocule`, `ions.incremental`, `tic`, `it.ms`, plus any additional columns defined in the `add` argument
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package="isoorbi")
#' df <- orbi_read_isox(file = fpath) |> orbi_simplify_isox()
#'
#' @export

orbi_simplify_isox <- function(dataset, add = c()) {
  # safety checks
  cols <- c(
    "filepath",
    "filename",
    "compound",
    "scan.no",
    "time.min",
    "isotopocule",
    "ions.incremental",
    "tic",
    "it.ms"
  )
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filepath`, `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`" = all(
      cols %in% names(dataset)
    )
  )

  if (!all(add %in% names(dataset))) {
    abort("not all `add` columns are in the dataset")
  }

  if (nrow(dataset) < 1) {
    abort("dataset contains no rows")
  }

  # info
  cols <- names(dataset)[names(dataset) %in% c(cols, add)]
  start <- start_info("is running")

  # select
  out <-
    dataset |>
    dplyr::select(!!!cols) |>
    try_catch_cnds(catch_errors = FALSE, catch_warnings = FALSE)

  # info
  finish_info(
    "kept column{?s} {.field {cols}}",
    start = start,
    conditions = out$conditions
  )

  # return
  return(out$result)
}

#' Filter isox files
#'
#' `r lifecycle::badge("deprecated")`
#' [orbi_filter_isox()] was renamed [orbi_filter_files()] to incorporate its wider scope for filtering both isox and raw files data.
#'
#' @param ... arguments passed on to [orbi_filter_files()]
#' @export
orbi_filter_isox <- function(...) {
  lifecycle::deprecate_soft(
    "1.5.0",
    "orbi_filter_isox()",
    "orbi_filter_files()",
    details = "`isoorbi` can now work with isox and raw files and this function was renamed accordingly"
  )
  orbi_filter_files(...)
}

#' Basic generic data files filter
#'
#' This is a basic filter function for file names, compounds and time ranges.
#' For filtering isotopocules, this function calls [orbi_filter_isotopocules()] internally (as of isoorbi version 1.5.0 [orbi_filter_isotopocules()] can also be used directly instead of via this function).
#' Default value for all parameters is NULL, i.e. no filter is applied.
#'
#'
#' @inheritParams orbi_flag_satellite_peaks
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
#'   orbi_filter_files(
#'     filenames = c("s3744"),
#'     compounds = "HSO4-",
#'     isotopocules = c("M0", "34S", "18O")
#'   )
#'
#' @return Filtered tibble
#' @export
orbi_filter_files <-
  function(
    dataset,
    filenames = NULL,
    compounds = NULL,
    isotopocules = NULL,
    time_min = NULL,
    time_max = NULL
  ) {
    # safety checks
    check_dataset_arg(dataset)
    stopifnot(
      "`filenames` must be a vector of filenames (or NULL)" = is.null(
        filenames
      ) ||
        is_character(filenames),
      "`compounds` must be a vector of compounds (or NULL)" = is.null(
        compounds
      ) ||
        is_character(compounds),
      "`isotopocules` must be a vector of isotopocules (or NULL)" = is.null(
        isotopocules
      ) ||
        is_character(isotopocules),
      "`time_min` must be a single number (or NULL)" = is.null(time_min) ||
        is_scalar_double(time_min),
      "`time_max` must be a single number (or NULL)" = is.null(time_max) ||
        is_scalar_double(time_max)
    )

    # column checks
    is_agg <- is(dataset, "orbi_aggregated_data")
    if (is_agg) {
      check_tibble(dataset$file_info, c("uidx", "filename"))
      check_tibble(dataset$scans, c("uidx", "scan.no"))
      check_tibble(dataset$peaks, c("uidx", "scan.no"))
      check_tibble(dataset$spectra, c("uidx", "scan.no"))
      check_tibble(dataset$problems, "uidx")
    } else {
      check_tibble(dataset, "filename")
    }

    # info
    filters <- c()
    if (!is.null(filenames)) {
      filters <- c(
        filters,
        sprintf("filenames (%s)", paste(filenames, collapse = ", "))
      )
    }
    if (!is.null(compounds)) {
      if (is_agg) {
        check_tibble(dataset$peaks, "compound")
      } else {
        check_tibble(dataset, "compound")
      }
      filters <- c(
        filters,
        sprintf("compounds (%s)", paste(compounds, collapse = ", "))
      )
    }
    if (!is.null(isotopocules)) {
      filters <- c(
        filters,
        sprintf("isotopocules (%s)", paste(isotopocules, collapse = ", "))
      )
    }
    if (!is.null(time_min)) {
      if (is_agg) {
        check_tibble(dataset$scans, "time.min")
      } else {
        check_tibble(dataset, "time.min")
      }
      filters <- c(filters, sprintf("minimum time (%s minutes)", time_min))
    }
    if (!is.null(time_max)) {
      if (is_agg) {
        check_tibble(dataset$scans, "time.min")
      } else {
        check_tibble(dataset, "time.min")
      }
      filters <- c(filters, sprintf("maximum time (%s minutes)", time_min))
    }

    # info
    n_row_start <- if (is_agg) nrow(dataset$peaks) else nrow(dataset)
    start <- start_info("is running")

    # filtering
    out <-
      try_catch_cnds(
        {
          # file: filenames
          if (!is.null(filenames)) {
            if (is_agg) {
              dataset$file_info <- dataset$file_info |>
                dplyr::filter(.data$filename %in% !!filenames) |>
                droplevels()
              dataset$scans <- dataset$scans |>
                dplyr::semi_join(dataset$file_info, by = "uidx")
              dataset$peaks <- dataset$peaks |>
                dplyr::semi_join(dataset$file_info, by = "uidx")
              dataset$spectra <- dataset$spectra |>
                dplyr::semi_join(dataset$file_info, by = "uidx")
              dataset$problems <- dataset$problems |>
                dplyr::semi_join(dataset$file_info, by = "uidx")
            } else {
              dataset <- dataset |>
                dplyr::filter(.data$filename %in% !!filenames) |>
                droplevels()
            }
          }

          # filter: compounds
          if (!is.null(compounds)) {
            if (is_agg) {
              dataset$peaks <- dataset$peaks |>
                dplyr::filter(.data$compound %in% !!compounds) |>
                droplevels()
            } else {
              dataset <- dataset |>
                dplyr::filter(.data$compound %in% !!compounds) |>
                droplevels()
            }
          }

          # filter: isotopocules
          if (!is.null(isotopocules)) {
            # use orbi_filter_isotopocules now
            dataset <- orbi_filter_isotopocules(
              dataset,
              isotopocules = isotopocules
            )
          }

          # filter: time_min
          if (!is.null(time_min)) {
            if (is_agg) {
              dataset$scans <- dataset$scans |>
                dplyr::filter(.data$time.min >= !!time_min)
              dataset$peaks <- dataset$peaks |>
                dplyr::semi_join(dataset$scans, by = c("uidx", "scan.no"))
              dataset$spectra <- dataset$spectra |>
                dplyr::semi_join(dataset$scans, by = c("uidx", "scan.no"))
            } else {
              dataset <- dataset |> dplyr::filter(.data$time.min >= !!time_min)
            }
          }

          # filter: time_max
          if (!is.null(time_max)) {
            if (is_agg) {
              dataset$scans <- dataset$scans |>
                dplyr::filter(.data$time.min <= !!time_max)
              dataset$peaks <- dataset$peaks |>
                dplyr::semi_join(dataset$scans, by = c("uidx", "scan.no"))
              dataset$spectra <- dataset$spectra |>
                dplyr::semi_join(dataset$scans, by = c("uidx", "scan.no"))
            } else {
              dataset <- dataset |> dplyr::filter(.data$time.min <= !!time_max)
            }
          }

          # return
          dataset
        }
      )

    # stop for errors
    abort_cnds(
      out$conditions,
      message = "something went wrong trying to filter dataset:"
    )
    dataset <- out$result

    # info
    n_row_end <- if (is_agg) nrow(dataset$peaks) else nrow(dataset)
    finish_info(
      "filtered the dataset by {.field {filters}} and removed ",
      "a total of {format_number(n_row_start - n_row_end)}/{format_number(n_row_start)} ",
      "peaks ({signif(100 * (n_row_start - n_row_end)/n_row_start, 2)}%)",
      start = start
    )

    # return
    return(dataset)
  }
