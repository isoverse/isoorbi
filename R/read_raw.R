# auxiliary raw file functions ========

#' Find raw files
#' @description Finds all .raw files in a folder.
#' @param folder path to a folder with raw files
#' @param recursive whether to find files recursively
#'
#' @examples
#'
#' # all .raw files provided with the isoorbi package
#' orbi_find_raw(system.file("extdata", package = "isoorbi"))
#'
#' @export
orbi_find_raw <- function(folder, recursive = TRUE) {
  # safety check
  stopifnot(
    "`folder` must be an existing directory" = dir.exists(folder),
    "`recursive` must be TRUE or FALSE" = is_scalar_logical(recursive)
  )

  # return
  list.files(
    folder,
    pattern = "\\.raw",
    full.names = TRUE,
    recursive = recursive
  )
}


# make sure raw reader is available
check_for_raw_reader <- function() {}

# read raw files =======

#' Read RAW files
#'
#' Read raw data files (`.raw`) from Orbitrap IRMS runs directly. This function extracts all available information and thus can be relatively slow (~1s / Mb on a typical personal computer). The results can be used directly or, more typically, are aggregated with [orbi_aggregate_raw()] to safely extract the relevant information for downstream processing.  This function is designed to be fail save by safely catching errors and reporting back on them (see [orbi_get_problems()]).
#'
#' @param file_paths paths to the `.raw` file(s), single value or vector of paths. Use [orbi_find_raw()] to get all raw files in a folder.
#' @param show_progress whether to show a progress bar, by default always enabled when running interactively e.g. inside RStudio (and disabled in a notebook), turn off with `show_progress = FALSE`
#' @param show_problems whether to show problems encountered along the way (rather than just keeping track of them with [orbi_get_problems()]). Set to `show_problems = FALSE` to turn off the live printout. Either way, all encountered problems can be retrieved with running [orbi_get_problems()] for the returned list
#' @return a tibble data frame where each row holds the file path and nested tibbles of datasets extracted from the raw file (typically `file_info`, `scans`, `peaks`, and `raw_data`). This is the safest way to extract the data without needing to make assumptions about compatibility across files. Extract your data of interest from the tibble columns or use [orbi_aggregate_raw()] to extract safely across files.
#' @export
orbi_read_raw <- function(
  file_paths,
  show_progress = rlang::is_interactive(),
  show_problems = TRUE
) {
  # get started
  call <- force(rlang::current_call())
  start <- Sys.time()

  # check for availability of rawrr
  if (!requireNamespace("rawrr", quietly = TRUE)) {
    cli_abort(
      c(
        "the {.pkg rwarr} package is required to read .raw files, please see {.url https://bioconductor.org/packages/rawrr/} for details",
        "i" = "typically, runnign the following commands will work to install {.pkg rawrr} and its assemblies:",
        " " = "{.emph install.packages(\"BiocManager\")}",
        " " = "{.emph BiocManager::install(\"rawrr\")}",
        " " = "{.emph rawrr::installRawFileReaderDLLs()}",
        " " = "{.emph rawrr::buildRawrrExe() || rawrr::installRawrrExe()}"
      )
    )
  }

  # safety checks
  stopifnot(
    "no file path supplied" = !missing(file_paths),
    "`file_paths` has to be at least one file path" = is_character(
      file_paths
    ) &&
      length(file_paths) >= 1L
  )

  # progress bar
  pb <- NULL
  n_files <- length(file_paths)
  if (show_progress) {
    old <- options(cli.progress_show_after = 0)
    on.exit(options(old))
    pb_multiplier <- 10 # for subprocesses
    pb <- cli_progress_bar(
      extra = list(
        file_path = file_paths[1],
        n_files = n_files,
        multiplier = pb_multiplier
      ),
      total = n_files * pb_multiplier,
      format = paste(
        "Reading {floor(pb_current/pb_extra$multiplier) + 1}/{pb_extra$n_files}",
        "raw files {pb_bar} {pb_percent}",
        "| {pb_elapsed} | ETA {pb_eta} | {.emph {basename(pb_extra$file_path)}}",
        "({prettyunits::pretty_bytes(file.size(pb_extra$file_path))})",
        "| step {pb_current %% pb_extra$multiplier + 1}: {.field {pb_status}}"
      )
    )
    on.exit(cli_process_done(id = pb), add = TRUE)
  }

  # read files safely and with progress info
  read_safely_with_progress <- function(file_path, i) {
    # progress
    if (show_progress) {
      cli_progress_update(
        id = pb,
        set = (i - 1) * 10,
        extra = list(file_path = file_path),
        status = "initializing"
      )
    }

    # read file and catch errors
    file_start <- Sys.time()
    out <-
      try_catch_cnds(
        read_raw_file(
          file_path,
          show_problems = FALSE, # summarized below
          show_progress = show_progress,
          pb = pb
        ),
        error_value = tibble(file_path = file_path, problems = list(tibble())),
        catch_errors = !orbi_get_option("debug")
      )

    # merge new into the returned problems
    problems <- bind_rows(out$result$problems, out$conditions)
    out$result$problems <- list(problems)

    # info
    info <- format_inline(
      if (nrow(problems) == 0) {
        "{col_green(symbol$tick)} "
      } else {
        "{col_green(symbol$info)} "
      },
      "{.timestamp {prettyunits::pretty_sec(as.numeric(Sys.time() - file_start, 'secs'))}} ",
      "Read {.emph {basename(file_path)}} ",
      if (file.exists(file_path)) {
        "({prettyunits::pretty_bytes(file.size(file_path))}) "
      },
      summarize_cnds(problems, "but encountered {issues}"),
      if (show_problems && nrow(problems) > 0) ":"
    )
    cli_text(info)

    # problems info
    if (show_problems) {
      show_cnds(problems, include_summary = FALSE, indent_cnds = TRUE)
    }

    # return
    return(out$result)
  }

  # read files (separate calls to simplify backtraces)
  results <- file_paths |>
    map2(seq_along(file_paths), read_safely_with_progress)
  if (show_progress) {
    cli_progress_done(id = pb)
  }
  results <- results |> bind_rows()

  # info
  problems <- results$problems |> dplyr::bind_rows()
  if (n_files > 1) {
    # if there's more than 1 file read
    info <- format_inline(
      "{col_green(symbol$tick)} ",
      "Read {n_files} raw file{?s} ",
      "in {prettyunits::pretty_sec(as.numeric(Sys.time() - start, 'secs'))} ",
      summarize_cnds(
        problems,
        "but encountered a total of {issues} {symbol$arrow_right} check with `orbi_get_problems(x)`"
      )
    )
    cli_text(info)
  }

  # return
  return(results)
}

# raw file reader
read_raw_file <- function(
  file_path,
  show_progress = rlang::is_interactive(),
  show_problems = TRUE,
  pb = NULL
) {
  # safety checks
  if (!file.exists(file_path)) {
    rlang::abort("file does not exist")
  }

  # headers
  if (show_progress) {
    cli_progress_update(id = pb, status = "read headers")
  }
  headers <- try_catch_cnds(
    rawrr::readFileHeader(file_path),
    error_value = tibble(),
    catch_errors = !orbi_get_option("debug")
  )
  parse_headers <- function() convert_list_to_tibble(headers$result)
  headers_parsed <- try_catch_cnds(
    parse_headers(),
    error_value = tibble(),
    catch_errors = !orbi_get_option("debug")
  )

  # indices
  if (show_progress) {
    cli_progress_update(id = pb, status = "indices")
  }
  indices <- try_catch_cnds(
    rawrr::readIndex(file_path),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )
  parse_indices <- function() {
    if (!"scan" %in% names(indices$result)) {
      rlang::abort("no {.var scan} column found in scan index")
    }
    indices$result$scan <- as.integer(indices$result$scan)
    return(tibble::as_tibble(indices$result))
  }
  indices_parsed <- try_catch_cnds(
    parse_indices(),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )

  # spectra
  if (show_progress) {
    cli_progress_update(id = pb, status = "spectra")
  }
  spectra <- try_catch_cnds(
    rawrr::readSpectrum(file_path, scan = indices$result$scan),
    error_value = list(),
    catch_errors = !orbi_get_option("debug")
  )

  # parse spectral data
  if (show_progress) {
    cli_progress_update(id = pb, status = "parse spectra")
  }
  parse_spectra <- function() {
    spectra <- spectra$result |>
      map(
        function(spectrum) {
          if (!"scan" %in% names(spectrum)) {
            rlang::abort("no {.var scan} column found in spectrum")
          }
          spectrum$scan <- as.integer(spectrum$scan)
          convert_list_to_tibble(spectrum)
        }
      )
    dplyr::bind_rows(spectra)
  }
  spectra_parsed <- try_catch_cnds(
    parse_spectra(),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )

  # scan info
  if (show_progress) {
    cli_progress_update(id = pb, status = "parse scan info")
  }
  parse_scan_info <- function() {
    scan_info_cols <- !grepl(
      "^(centroid\\.|mZ|intensity)",
      names(spectra_parsed$result)
    )
    scan_info <- spectra_parsed$result[, scan_info_cols]
    scan_cols <- setdiff(names(indices_parsed$result), names(scan_info))
    indices_parsed$result[c("scan", scan_cols)] |>
      dplyr::left_join(scan_info, by = "scan")
  }
  scan_info <- try_catch_cnds(
    parse_scan_info(),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )

  # peaks
  if (show_progress) {
    cli_progress_update(id = pb, status = "parse peaks")
  }
  parse_peaks <- function() {
    peak_cols <- grepl("^centroid\\.", names(spectra_parsed$result))
    if (!any(peak_cols)) {
      rlang::abort("no centroid data found in spectra, returning empty peaks")
    }
    peaks <-
      spectra_parsed$result[
        names(spectra_parsed$result) == "scan" | peak_cols
      ] |>
      tibble::as_tibble()
    peaks |> tidyr::unnest(-"scan")
  }
  peaks <- try_catch_cnds(
    parse_peaks(),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )

  # raw data
  if (show_progress) {
    cli_progress_update(id = pb, status = "parse raw data")
  }
  parse_raw_data <- function() {
    data_cols <- grepl("^(mZ|intensity)", names(spectra_parsed$result))
    if (!any(data_cols)) {
      rlang::abort(
        "no raw data ({.var mZ} & {.var intensity}) found in spectra, returning empty raw data table"
      )
    }
    raw_data <-
      spectra_parsed$result[
        names(spectra_parsed$result) == "scan" | data_cols
      ] |>
      tibble::as_tibble()
    raw_data |> tidyr::unnest(-"scan")
  }
  raw_data <- try_catch_cnds(
    parse_raw_data(),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )

  # wrapping up
  if (show_progress) {
    cli_progress_update(id = pb, status = "finalizing")
  }

  # problems
  problems <- bind_rows(
    headers$conditions,
    headers_parsed$conditions,
    indices$conditions,
    indices_parsed$conditions,
    spectra$conditions,
    spectra_parsed$conditions,
    scan_info$conditions,
    peaks$conditions,
    raw_data$conditions
  )
  if (show_problems) {
    show_cnds(problems)
  }

  # combine
  out <-
    tibble(
      file_path = file_path,
      file_info = list(headers_parsed$result),
      scans = list(scan_info$result),
      peaks = list(peaks$result),
      raw_data = list(raw_data$result),
      problems = list(problems)
    )

  # if there's a caching mechanism, it should go here

  # return
  return(out)
}

# utility function ==========

# turns multi value entries into list columns and omits NULL values
convert_list_to_tibble <- function(file_info_list) {
  lens <- lengths(file_info_list)
  file_info_list[lens > 1] <- lapply(file_info_list[lens > 1], list)
  return(tibble::as_tibble(file_info_list[lens > 0]))
}
