# auxiliary raw file functions ========

#' Find raw files
#' @description Finds all .raw files in a folder.
#' @param folder path to a folder with raw files
#' @param include_cache whether to include .raw.cache folders in the absence of the corresponding .raw file so that copies of the cache are read even in the absence of the original raw files
#' @param recursive whether to find files recursively
#'
#' @examples
#'
#' # all .raw files provided with the isoorbi package
#' orbi_find_raw(system.file("extdata", package = "isoorbi"))
#'
#' @export
orbi_find_raw <- function(folder, include_cache = TRUE, recursive = TRUE) {
  # safety checks
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

  # raw files
  files <- list.files(
    folder,
    pattern = "\\.raw$",
    full.names = TRUE,
    recursive = recursive
  ) |>
    unique()

  # cached folders
  if (include_cache) {
    folders <- list.files(
      folder,
      pattern = "\\.raw\\.cache$",
      full.names = TRUE,
      include.dirs = TRUE,
      recursive = recursive
    ) |>
      unique()
    folders <- folders[dir.exists(folders)]
    if (length(folders) > 0) {
      # include folders
      linked_files <- gsub("\\.raw\\.cache$", ".raw", folders)
      folders <- folders[!linked_files %in% files]
      files <- c(files, folders) |> unique() |> sort()
    }
  }

  # return
  return(files)
}

# make sure raw reader is available
# note: this should really call rawrr:::.isAssemblyWorking()
# but it's not exported so we have to read a test file to
# triger it --> could save some processing time here,
# consider reimplementing rawrr::.isAssemblyWorking() for
# this purpose
check_for_raw_reader <- function(
  install_if_missing = TRUE,
  call = caller_env()
) {
  # check for availability of rawrr
  if (!requireNamespace("rawrr", quietly = TRUE)) {
    cli_abort(
      c(
        "the {.pkg rwarr} package is required to read .raw files, please see {.url https://bioconductor.org/packages/rawrr/} for details",
        "i" = "typically, running the following commands will work to install {.pkg rawrr}:",
        " " = "{.emph install.packages(\"BiocManager\")}",
        " " = "{.emph BiocManager::install(\"rawrr\")}"
      ),
      call = call
    )
  }
  tryCatch(
    # read a minimal file as a test
    # note: index is fastest
    rawrr::readIndex(system.file(
      "extdata",
      "nitrate_test_1scan.raw",
      package = "isoorbi"
    )),
    error = function(cnd) {
      # missing rawrrr exe
      if (grepl("rawrr::installRawrrExe", conditionMessage(cnd))) {
        if (install_if_missing) {
          # try to install
          cli_alert_warning(
            "Could not find the executable for the RAW file reader:"
          )
          cli_inform(c(" " = "{.emph {cnd$message}}"))
          cli_inform(c(
            ">" = "Trying to install {.pkg rawrr.exe} (this requires an internet connection and may take a moment)..."
          ))
          rawrr::installRawrrExe()
          return(check_for_raw_reader(install_if_missing = FALSE, call = call))
        } else {
          # failed to install
          cli_abort(
            "Failed to install the executable, please try manually",
            parent = cnd,
            call = call
          )
        }
      } else {
        cli_abort(
          "Could not run the executable for the RAW file reader",
          parent = cnd,
          call = call
        )
      }
    }
  )
}

# read raw files =======

#' Read RAW files
#'
#' Read raw data files (`.raw`) from Orbitrap IRMS runs directly. This function extracts all available information and thus can be relatively slow (~1s / Mb on a typical personal computer) but with the caching this is only true the first time. The results can be used directly or, more typically, are aggregated with [orbi_aggregate_raw()] to safely extract the relevant information for downstream processing.  This function is designed to be fail save by safely catching errors and reporting back on them (see [orbi_get_problems()]).
#'
#' @param file_paths paths to the `.raw` file(s), single value or vector of paths. Use [orbi_find_raw()] to get all raw files in a folder.
#' @param show_progress whether to show a progress bar, by default always enabled when running interactively e.g. inside RStudio (and disabled in a notebook), turn off with `show_progress = FALSE`
#' @param show_problems whether to show problems encountered along the way (rather than just keeping track of them with [orbi_get_problems()]). Set to `show_problems = FALSE` to turn off the live printout. Either way, all encountered problems can be retrieved with running [orbi_get_problems()] for the returned list
#' @param include_spectra whether to include the whole spectral data in the returned tibble (makes it much larger and takes longer to read in)
#' @param cache whether to automatically cache the read raw files (writes highly efficient .parquet files in a folder with the same name as the file .cache appended)
#' @param cache_spectra whether to automatically cache requested scan spectra (this can take up significant disc space)
#' @param read_cache whether to read the file from cached .parquet files (if they exist) or anew
#' @return a tibble data frame where each row holds the file path and nested tibbles of datasets extracted from the raw file (typically `cache_info`, `scans`, `peaks`, and `spectra`). This is the safest way to extract the data without needing to make assumptions about compatibility across files. Extract your data of interest from the tibble columns or use [orbi_aggregate_raw()] to extract safely across files.
#' @export
orbi_read_raw <- function(
  file_paths,
  show_progress = rlang::is_interactive(),
  show_problems = TRUE,
  include_spectra = orbi_get_option("include_spectra"),
  cache = TRUE,
  cache_spectra = cache,
  read_cache = TRUE
) {
  # keep track of current env to anchor progress bars
  root_env <- current_env()

  # check for raw file reader
  check_for_raw_reader()

  # safety checks
  check_arg(
    file_paths,
    !missing(file_paths) &&
      is_character(file_paths) &&
      length(file_paths) > 0,
    "must be at least one file path"
  )

  # check what kind of spectra are requested
  if (identical(include_spectra, TRUE)) {
    # all
    all_spectra <- TRUE
    select_spectra <- NULL
  } else if (identical(include_spectra, FALSE)) {
    # none
    all_spectra <- FALSE
    select_spectra <- integer()
  } else {
    # some subset
    all_spectra <- FALSE
    select_spectra <- as.integer(include_spectra) |> na.omit()
  }

  # keep track of total time
  start_time <- Sys.time()

  # generate caching info (also checks that files exist)
  cache_info <- get_cache_info(
    file_paths = unique(file_paths),
    read_cache = read_cache
  )

  # read files from cache
  read_safely_from_cache <- function(info) {
    # progress
    if (!is.null(start$pb) && info$idx > 1) {
      cli_progress_update(
        id = start$pb,
        set = info$step,
        extra = list(info = info),
        status = "initializing",
        .envir = root_env
      )
    }

    # read file and catch errors
    file_start <- start_info()
    out <-
      try_catch_cnds(
        read_cached_raw_file(
          cache_info = info,
          all_spectra = all_spectra,
          select_spectra = select_spectra,
          cache = cache,
          cache_spectra = cache_spectra,
          pb = start$pb,
          .env = root_env
        ),
        error_value = tibble(),
        catch_errors = !orbi_get_option("debug")
      )

    # info
    finish_info(
      if (ncol(out$result) == 0) {
        "tried to read {.file {basename(info$file_path)}} from cache"
      } else if (nrow(out$conditions) > 0) {
        "read {.file {basename(info$file_path)}} partially from cache"
      } else {
        "read {.file {basename(info$file_path)}} from cache"
      },
      start = file_start,
      conditions = out$conditions,
      show_conditions = show_problems,
      .call = expr(orbi_read_raw())
    )

    # include idx in the result and indicate whether file should be reread
    return(
      out$result |>
        dplyr::mutate(idx = info$idx, read = nrow(out$conditions) > 0)
    )
  }

  # read cached files first
  if (any(cache_info$is_cached)) {
    # subset files to read just the cached ones
    cached_files <- cache_info |>
      dplyr::filter(.data$is_cached) |>
      dplyr::mutate(
        # progress bar info
        step = cumsum(dplyr::lag(.data$read_file_size, default = 0)),
        total = sum(.data$read_file_size),
      )

    # info
    start <- start_info(
      "is reading {pb_extra$info$idx}/{pb_extra$n_files} cached raw files {pb_bar} ",
      "| {pb_elapsed} | ETA {pb_eta} | {.file {basename(pb_extra$info$file_path)}} ",
      "| {.field {pb_status}}",
      pb_total = cached_files$total[1],
      pb_extra = list(info = cached_files[1, ], n_files = nrow(cached_files)),
      pb_status = "initializing",
      show_progress = show_progress,
      .env = root_env
    )

    # read cached files
    results <-
      cached_files |>
      split(1:nrow(cached_files)) |>
      purrr::map(read_safely_from_cache) |>
      dplyr::bind_rows()
    finish_info(start = start)

    # indicate which files need to be read
    all_files <- cache_info |>
      dplyr::left_join(
        results |>
          # tidyr::nest(
          #   from_cache = c("file_info", "scans", "peaks", "spectra", "problems")
          # ) |>
          dplyr::select("idx", "read"),
        by = "idx"
      ) |>
      dplyr::mutate(read = is.na(.data$read) | .data$read)
  } else {
    # nothing cached, read all
    all_files <- cache_info |>
      dplyr::mutate(read = TRUE, from_cache = list(NULL))
  }

  return(results)

  # read raw files safely
  read_safely <- function(info) {
    # progress
    if (!is.null(start$pb) && info$idx > 1) {
      cli_progress_update(
        id = start$pb,
        set = info$step,
        extra = list(info = info),
        status = "initializing",
        .envir = root_env
      )
    }

    # read file and catch errors
    file_start <- start_info()
    out <-
      try_catch_cnds(
        read_raw_file(
          cache_info = info,
          all_spectra = all_spectra,
          select_spectra = select_spectra,
          from_cache = info$from_cache[[1]],
          cache = cache,
          pb = start$pb,
          .env = root_env
        ),
        error_value = tibble(
          file_path = info$file_path,
          problems = list(tibble())
        ),
        catch_errors = !orbi_get_option("debug")
      )

    # merge new into the returned problems
    problems <- bind_rows(out$result$problems, out$conditions)
    out$result$problems <- list(problems)

    # file read info
    finish_info(
      "{if(info$is_cached) 're-read' else 'read'} ",
      "{.file {basename(info$file_path)}} ",
      "({prettyunits::pretty_bytes(info$file_size)})",
      start = file_start,
      conditions = out$result$problems[[1]],
      show_conditions = show_problems,
      .call = expr(orbi_read_raw())
    )

    # include idx in the result
    return(
      out$result |>
        dplyr::mutate(idx = info$idx)
    )
  }

  # any files to read?
  if (any(all_files$read)) {
    read_files <- all_files |>
      dplyr::filter(.data$read) |>
      dplyr::mutate(
        # progress bar info
        step = cumsum(dplyr::lag(.data$read_file_size, default = 0)),
        total = sum(.data$read_file_size)
      )

    # info / progress
    start <- start_info(
      "is reading {pb_extra$info$idx}/{pb_extra$n_files} raw files {pb_bar} ",
      "| {pb_elapsed} | ETA {pb_eta} | {.file {basename(pb_extra$info$file_path)}} ",
      "({prettyunits::pretty_bytes(file.size(pb_extra$info$file_path))}) ",
      "| {.field {pb_status}}",
      pb_total = read_files$total[1],
      pb_extra = list(info = read_files[1, ], n_files = nrow(read_files)),
      pb_status = "initializing",
      show_progress = show_progress,
      .env = root_env
    )

    # read files
    results <-
      read_files |>
      split(1:nrow(read_files)) |>
      purrr::map(read_safely) |>
      dplyr::bind_rows()
    finish_info(start = start)
  } else {
    results <- tibble()
  }

  # get all data
  all_files <-
    all_files |>
    dplyr::filter(!.data$read) |>
    dplyr::select("idx", "file_path", "from_cache") |>
    tidyr::unnest(.data$from_cache) |>
    dplyr::select(-dplyr::any_of("from_cache")) |>
    dplyr::bind_rows(results) |>
    dplyr::arrange(.data$idx)

  # info
  problems <- all_files$problems |> dplyr::bind_rows()
  if (nrow(all_files) > 1) {
    # if there's more than 1 file --> post a summary
    finish_info(
      "finished reading {nrow(all_files)} file{?s} ",
      if (nrow(problems) > 0) {
        # custom problems summary
        summarize_cnds(
          problems,
          include_symbol = FALSE,
          include_call = FALSE,
          summary_format = "but encountered a total of {issues} {symbol$arrow_right} check with {.strong orbi_get_problems(x)}"
        )
      },
      success_format = if (nrow(problems) > 0) {
        "{col_yellow('!')} {msg}"
      } else {
        "{col_green(symbol$tick)} {msg}"
      },
      start = list(start_time = start_time)
    )
  }

  # return
  return(all_files)
}

# fetch information about file caching
get_cache_info <- function(
  file_paths,
  read_cache = TRUE
) {
  # safety checks
  if (any(missing <- !file.exists(file_paths))) {
    cli_abort(
      c(
        "{qty(sum(missing))} file{?s} {?does/do} not exist",
        basename(file_paths[missing]) |> set_names("x")
      )
    )
  }

  # read existing cache info
  empty_info <- tibble(
    old_file_size = integer(),
    old_isoorbi_version = character(),
    old_cache_timestamp = as.POSIXct(integer())
  )
  read_cache_info <- function(is_cached, cache_info_path) {
    if (!is_cached) {
      # not cached
      return(empty_info)
    }
    cache_info <- try_catch_cnds(arrow::read_parquet(cache_info_path))
    if (nrow(cache_info$conditions) > 0) {
      # something wrong with the cache info
      return(empty_info)
    }
    req_cols <- gsub("old_", "", names(empty_info))
    if (!all(req_cols %in% names(cache_info$result))) {
      # something's amiss in the cache info
      return(empty_info)
    }
    return(
      cache_info$result |> set_names(paste0("old_", names(cache_info$result)))
    )
  }

  # caching info
  cache_info <-
    tibble(
      idx = seq_along(!!file_paths),
      file_path = gsub("\\.cache$", "", !!file_paths),
      file_exists = file.exists(.data$file_path),
      file_size = as.integer(file.size(.data$file_path)),
      cache_path = paste0(.data$file_path, ".cache"),
      cache_info = file.path(.data$cache_path, "cache_info.parquet"),
      file_info = file.path(.data$cache_path, "file_info.parquet"),
      scans = file.path(.data$cache_path, "scans.parquet"),
      peaks = file.path(.data$cache_path, "peaks.parquet"),
      problems = file.path(.data$cache_path, "problems.parquet"),
      spectra = file.path(.data$cache_path, "spectra.parquet"),
      is_cached = if (!!read_cache) {
        file.exists(.data$cache_info) &
          file.exists(.data$file_info) &
          file.exists(.data$scans) &
          file.exists(.data$peaks) &
          file.exists(.data$problems)
      } else {
        FALSE
      },
      has_cached_spectra = if (!!read_cache) {
        file.exists(.data$spectra)
      } else {
        FALSE
      },
      # read existing cache info
      info = purrr::map2(.data$is_cached, .data$cache_info, read_cache_info)
    ) |>
    # unpack old info
    tidyr::unnest(.data$info, keep_empty = TRUE) |>
    dplyr::mutate(
      # use cached scans + peaks to estimate file size read for cache reads
      read_file_size = dplyr::case_when(
        !.data$is_cached ~ .data$file_size,
        .data$has_cached_spectra ~
          file.size(.data$scans) + file.size(.data$spectra),
        TRUE ~ file.size(.data$scans)
      ),
      .after = "file_size"
    )

  # double check if there is a problem for rereads
  # most of the time this is caught earlier except if cache paths are passed in!
  missing <- !cache_info$is_cached & !cache_info$file_exists
  if (any(missing)) {
    cli_abort(
      c(
        "{qty(sum(missing))} file{?s} {?does/do} not exist but are required for re-reads",
        basename(file_paths[missing]) |> set_names("x")
      )
    )
  }

  # return
  return(cache_info)
}

# reader for cached raw files
# @param file_path can be used for a direct call (usually for testing only)
# @param cache_info typically called with this parameter internally
read_cached_raw_file <- function(
  cache_info = get_cache_info(
    file_path,
    read_cache = read_cache
  ),
  file_path = NULL,
  all_spectra = FALSE,
  select_spectra = integer(0),
  read_cache = TRUE,
  cache = TRUE,
  cache_spectra = TRUE,
  pb = NULL,
  .env = caller_env()
) {
  # safety checks
  check_arg(
    cache_info,
    is.data.frame(cache_info) && nrow(cache_info) == 1,
    "must be a single line data frame"
  )
  if (!cache_info$is_cached) {
    cli_abort(
      "there's no cache to read for {.file {basename(cache_info$file_path)}}",
      .internal = TRUE
    )
  }

  # check about file size
  if (
    !is.na(cache_info$file_size) &&
      cache_info$file_size != cache_info$old_file_size
  ) {
    cli_warn(
      "file size has changed (from {prettyunits::pretty_bytes(cache_info$old_file_size)} to {prettyunits::pretty_bytes(cache_info$file_size)}), cache is outdated"
    )
    return(tibble())
  }

  # check about isoorbi version
  # can use cache_info$old_isoorbi_version to determine whether a new read is necessary

  # simplify progress updates
  update_progress <- function(status) {
    if (!is.null(pb)) {
      cli_progress_update(
        id = pb,
        inc = 0,
        status = status,
        .envir = .env
      )
    }
  }

  # read file_info from cache
  update_progress("reading cached file info")
  file_info <- try_catch_cnds(arrow::read_parquet(cache_info$file_info))
  if (nrow(file_info$conditions) > 0) {
    cli_abort("failed to read cached file info")
  }

  # read scans from cache
  update_progress("reading cached scans")
  scans <- try_catch_cnds(arrow::read_parquet(cache_info$scans))
  if (nrow(scans$conditions) > 0) {
    cli_abort("failed to read cached scans")
  }

  # read peaks from cache
  update_progress("reading cached peaks")
  peaks <- try_catch_cnds(arrow::read_parquet(cache_info$peaks))
  if (nrow(peaks$conditions) > 0) {
    cli_abort("failed to read cached peaks")
  }

  # read problems from cache
  update_progress("reading cached problems")
  problems <- try_catch_cnds(arrow::read_parquet(cache_info$problems))
  if (nrow(problems$conditions) > 0) {
    cli_abort("failed to read cached problems")
  }

  # read spectra from cache
  update_progress("reading cached spectra")
  spectral_data <- list(
    result = tibble(scan = integer()),
    conditions = tibble()
  )

  # are any spectra actually requested?
  if (all_spectra || length(select_spectra) > 0) {
    # read what's in the cache
    if (cache_info$has_cached_spectra) {
      spectral_data <- try_catch_cnds(arrow::read_parquet(cache_info$spectra))
      if (nrow(spectral_data$conditions) > 0) {
        cli_abort("failed to read cached spectra")
      }
    }

    # is what we got enough?
    missing_spectra <-
      if (all_spectra) {
        scans$result$scan |> setdiff(spectral_data$result$scan)
      } else {
        select_spectra |>
          intersect(scans$result$scan) |>
          setdiff(spectral_data$result$scan)
      }

    # no --> let's see if we can get the missing spectra
    if (length(missing_spectra) > 0) {
      cli_warn(
        "{length(missing_spectra)} spectr{?um/a} {?is/are} not yet cached"
      )
    }

    # got_all <- setdiff(scans$result$scan, spectral_data$result$scan) == 0
    # req_spectra <- if (all_spectra) scans$result$scan
    # if (
    #   all_spectra &&
    #     length(setdiff(scans$result$scan, cached_spectral_data$result$scan)) > 0
    # ) {
    #   cli_warn("not all spectra are cached yet")
    # } else if (!all_spectra) {
    #   missing_spectra <- select_spectra |>
    #     intersect(scans$result$scan) |>
    #     setdiff(cached_spectral_data$result$scan)
    #   if (length(missing_spectra) > 0) {
    #     cli_warn("spectr{?um/a} {?is/are} not yet cached: {missing_spectra}")
    #   }
    # }
    # spectral_data <- cached_spectral_data

    # subset what we got if we don't want all
    if (!all_spectra) {
      spectral_data$result <- spectral_data$result |>
        dplyr::filter(.data$scan %in% !!select_spectra)
    }
  }

  # return
  tibble(
    file_path = cache_info$file_path,
    file_info = list(file_info$result),
    scans = list(scans$result),
    peaks = list(peaks$result),
    spectra = list(spectral_data$result),
    # add condition column (since it is never cached)
    problems = list(problems$result |> dplyr::mutate(condition = list(list())))
  )
}

# raw file reader
# @param file_path can be used for a direct call (usually for testing only)
# @param cache_info typically called with this parameter internally
read_raw_file <- function(
  cache_info = get_cache_info(
    file_path,
    read_cache = read_cache
  ),
  file_path = NULL,
  all_spectra = FALSE,
  select_spectra = integer(0),
  read_cache = TRUE,
  from_cache = tibble(),
  cache = TRUE,
  pb = NULL,
  .env = caller_env()
) {
  # safety checks
  if (!cache_info$file_exists) {
    cli_abort("cannot find original .raw file")
  }

  # do we need to create the cache path?
  if (cache && !dir.exists(cache_info$cache_path)) {
    dir.create(cache_info$cache_path)
  }

  # simplify progress updates
  update_progress <- function(status) {
    if (!is.null(pb)) {
      cli_progress_update(
        id = pb,
        inc = 0,
        status = status,
        .envir = .env
      )
    }
  }

  # start problems
  problems <- tibble() |> bind_rows(from_cache$problems[[1]])

  # read headers anew if not already available from cache
  file_info <- from_cache$file_info[[1]]
  if (is.null(file_info)) {
    # wrapper for executing rawrr reading functions
    # to catch issues with the underlying rawfile reader
    # NOTE: this may become easier in rawrr version 1.17 with a parameter to
    # readFileHeader for the stderr/out (set to true and then capture.output)
    # see https://github.com/fgcz/rawrr/issues/92
    read_headers <- function(call = caller_call()) {
      tryCatch(
        rawrr::readFileHeader(cache_info$file_path),
        error = function(cnd) {
          if (
            grepl("failed for an unknown reason", conditionMessage(cnd)) &&
              grepl("Please check the debug files", conditionMessage(cnd))
          ) {
            cli_abort(
              "the Thermo RawFileReader could not parse this file, it may not be a valid RAW file",
              call = call
            )
          }
          stop(cnd)
        }
      )
    }

    ## read headers
    update_progress("reading headers")
    headers <- try_catch_cnds(
      read_headers(),
      error_value = tibble(),
      catch_errors = !orbi_get_option("debug")
    )
    problems <- bind_rows(problems, headers$conditions)

    if (nrow(headers$conditions) > 0) {
      # this raw file cannot be read by the RawFileReader
      # --> return what we have and leave it at that
      return(
        tibble(
          file_path = cache_info$file_path,
          problems = problems
        )
      )
    }

    ## parse headers
    update_progress("parsing headers")
    parse_headers <- function() convert_list_to_tibble(headers$result)
    headers_parsed <- try_catch_cnds(
      parse_headers(),
      error_value = tibble(),
      catch_errors = !orbi_get_option("debug")
    )
    problems <- bind_rows(problems, headers_parsed$conditions)
    file_info <- headers_parsed$result

    ## cache headers and cache info
    if (cache) {
      arrow::write_parquet(
        tibble(
          file_size = as.integer(cache_info$file_size),
          isoorbi_version = as.character(packageVersion("isoorbi")),
          cache_timestamp = Sys.time()
        ),
        sink = cache_info$cache_info
      )
      arrow::write_parquet(file_info, sink = cache_info$file_info)
    }
  }

  ## read indices (this is always needed if we don't have everything cached yet)
  update_progress("reading indices")
  indices <- try_catch_cnds(
    rawrr::readIndex(cache_info$file_path),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )
  problems <- bind_rows(problems, indices$conditions)

  ## parse indices
  update_progress("parsing indices")
  parse_indices <- function() {
    if (!"scan" %in% names(indices$result)) {
      cli_abort("no {.var scan} column found in scan index")
    }
    indices$result$scan <- as.integer(indices$result$scan)
    return(tibble::as_tibble(indices$result))
  }
  indices_parsed <- try_catch_cnds(
    parse_indices(),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )
  problems <- bind_rows(problems, indices_parsed$conditions)

  ## read spectra (always needed to get all scan info even if spectra not returned)
  update_progress("reading spectra")
  spectra_raw <- try_catch_cnds(
    rawrr::readSpectrum(
      cache_info$file_path,
      scan = indices_parsed$result$scan
    ),
    error_value = list(),
    catch_errors = !orbi_get_option("debug")
  )
  problems <- bind_rows(problems, spectra_raw$conditions)

  ## parse spectral data
  update_progress("parse spectra")
  parse_spectra <- function() {
    spectra_raw$result |>
      map(
        function(spectrum) {
          if (!"scan" %in% names(spectrum)) {
            rlang::abort("no {.var scan} column found in spectrum")
          }
          spectrum$scan <- as.integer(spectrum$scan)
          convert_list_to_tibble(spectrum)
        }
      ) |>
      dplyr::bind_rows()
  }
  spectra_parsed <- try_catch_cnds(
    parse_spectra(),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )
  problems <- bind_rows(problems, spectra_parsed$conditions)

  # read scans anew if not already available from cache
  scans <- from_cache$scans[[1]]
  if (is.null(scans)) {
    ## parse scans info
    update_progress("parsing scans")
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
    indices_w_scan_info <- try_catch_cnds(
      parse_scan_info(),
      error_value = tibble(scan = integer(0)),
      catch_errors = !orbi_get_option("debug")
    )
    problems <- bind_rows(problems, indices_w_scan_info$conditions)
    scans <- indices_w_scan_info$result

    ## cache scan info
    if (cache) {
      arrow::write_parquet(scans, sink = cache_info$scans)
    }
  }

  # read peaks anew if not already available from cache
  peaks <- from_cache$peaks[[1]]
  if (is.null(peaks)) {
    ## parsing peaks
    update_progress("parsing peaks")
    parse_peaks <- function() {
      peak_cols <- grepl("^centroid\\.", names(spectra_parsed$result))
      if (!any(peak_cols)) {
        rlang::abort(
          "no centroid data found in spectra, returning empty peaks"
        )
      }
      spectra_parsed$result[
        names(spectra_parsed$result) == "scan" | peak_cols
      ] |>
        tibble::as_tibble() |>
        tidyr::unnest(-"scan")
    }
    parsed_peaks <- try_catch_cnds(
      parse_peaks(),
      error_value = tibble(scan = integer(0)),
      catch_errors = !orbi_get_option("debug")
    )
    problems <- bind_rows(problems, parsed_peaks$conditions)
    peaks <- parsed_peaks$result

    ## cache peaks
    if (cache) {
      arrow::write_parquet(peaks, sink = cache_info$peaks)
    }
  }

  # decide what spectra to return
  spectra <- tibble(scan = integer(0))
  if (all_spectra || length(select_spectra) > 0) {
    update_progress("selecting spectra")
    parse_spectral_data <- function() {
      data_cols <- grepl("^(mZ|intensity)", names(spectra_parsed$result))
      if (!any(data_cols)) {
        cli_abort(
          "no raw data ({.var mZ} & {.var intensity}) found in spectra, returning empty raw data table"
        )
      }
      spectra_parsed$result[
        names(spectra_parsed$result) == "scan" | data_cols
      ] |>
        tibble::as_tibble() |>
        # everything OR the requested ones + what is already in the cache
        dplyr::filter(
          !!all_spectra |
            .data$scan %in%
              union(!!select_spectra, from_cache$spectra[[1]]$scan)
        ) |>
        tidyr::unnest(-"scan")
    }
    spectral_data <- try_catch_cnds(
      parse_spectral_data(),
      error_value = tibble(scan = integer(0)),
      catch_errors = !orbi_get_option("debug")
    )
    problems <- bind_rows(problems, spectral_data$conditions)
    spectra <- spectral_data$result
    my_spectra <<- spectral_data
    my_problems <<- problems

    ## cache raw spectral data
    if (cache) {
      arrow::write_parquet(spectra, sink = cache_info$spectra)
    }
  }

  Sys.sleep(0.5) # FIXME
  if (grepl("_10scans", cache_info$file_path)) {
    cli_abort("something totally wrong")
  } # FIXME

  # wrapping up
  update_progress("finalizing")
  problems <- problems |>
    dplyr::filter(
      .by = c("type", "call", "message"),
      dplyr::n() == 1L | !purrr::map_lgl(.data$condition, is_empty)
    )

  # cache problems
  if (cache) {
    problems |>
      # CANNOT cache the actual error ojects
      dplyr::select(-"condition") |>
      arrow::write_parquet(sink = cache_info$problems)
  }

  # combine
  out <-
    tibble(
      file_path = cache_info$file_path,
      file_info = list(file_info),
      scans = list(scans),
      peaks = list(peaks),
      spectra = list(spectra),
      problems = list(problems)
    )

  # return
  return(out)
}

# utility function ==========

# turns multi value entries into list columns and omits NULL values
convert_list_to_tibble <- function(cache_info_list) {
  lens <- lengths(cache_info_list)
  cache_info_list[lens > 1] <- lapply(cache_info_list[lens > 1], list)
  return(tibble::as_tibble(cache_info_list[lens > 0]))
}
