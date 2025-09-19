# raw file reader installation ========

#' Check for the isoorbi raw file reader
#'
#' By default, this will install the isoraw reader if it is missing or outdated.
#'
#' @param install_if_missing install the reader if it's missing
#' @param reinstall_if_outdated install the reader if it's outdated (i.e. not at least `min_version`)
#' @param reinstall_always whether to (re-)install no matter what
#' @param min_version the minimum version number required
#' @param source_url the URL where to find the raw file reader, by default this is the latests release of the executables on github
#' @param ... passed on to `download.file` if (re-) installing the reader
#' @examples
#'
#'
#'
#' @export
orbi_check_isoraw <- function(
  install_if_missing = TRUE,
  reinstall_if_outdated = TRUE,
  reinstall_always = FALSE,
  min_version = "0.2.0",
  source_url = "https://github.com/isoverse/isoorbi/releases/download/isoraw-v0.2.0",
  ...
) {
  # start
  start <- start_info()

  # check existence
  isoraw_exists <- file.exists(get_isoraw_path())
  outdated <- FALSE

  # version function
  get_isoraw_version <- function() {
    if (!file.exists(get_isoraw_path())) {
      return(NULL)
    }
    version <- system2(
      get_isoraw_path(),
      args = c("--version"),
      stdout = TRUE,
      stderr = TRUE
    )

    if (
      !is_scalar_character(version) ||
        !grepl("isoraw version", version, fixed = TRUE)
    ) {
      cli_bullets(
        c(
          "!" = "Could not determine isoorbi raw file reader version, reader returned:",
          version |>
            purrr::map_chr(~ format_inline("{.emph {col_red(.x)}}")) |>
            format_bullets_raw() |>
            set_names(" ")
        )
      )
      return(NULL)
    }
    # return
    regmatches(version, regexpr("\\d+(\\.\\d+)*", version)) |> numeric_version()
  }

  # version
  if (isoraw_exists) {
    isoraw_version <- get_isoraw_version()
    if (
      is.null(isoraw_version) || isoraw_version < numeric_version(min_version)
    ) {
      outdated <- TRUE
      if (!is.null(isoraw_version)) {
        cli_bullets(
          c(
            "!" = "isoraw is outdated",
            "i" = "found version {isoraw_version} but need at least version {numeric_version(min_version)}"
          )
        )
      }
    }
  }

  # do we need to install?
  if (
    reinstall_always ||
      (!isoraw_exists && install_if_missing) ||
      (outdated && reinstall_if_outdated)
  ) {
    # yes we do
    cli_inform(c(
      ">" = "Trying to {if (isoraw_exists) 're'}install the new isoorbi raw file reader for your operating system {.pkg {basename(get_isoraw_path())}} (this requires an internet connection and may take a moment)..."
    ))

    # create folder
    dir.create(
      dirname(get_isoraw_path()),
      recursive = TRUE,
      showWarnings = FALSE
    )

    # download
    tryCatch(
      {
        # download to temp file
        tmpfile <- tempfile()
        download.file(
          file.path(source_url, basename(get_isoraw_path())),
          destfile = tmpfile,
          mode = "wb",
          ...
        )
        # remove the existing one
        if (isoraw_exists) {
          unlink(get_isoraw_path())
        }
        # move the downloaded one
        file.rename(tmpfile, get_isoraw_path())
      },
      error = function(cnd) {
        cli_abort(
          "could not download the isoorbi raw file reader",
          parent = cnd
        )
      }
    )

    # make executable
    Sys.chmod(get_isoraw_path(), mode = "0777", use_umask = TRUE)

    # get new version
    isoraw_version <- get_isoraw_version()
    if (!is.null(isoraw_version)) {
      finish_info(
        "successfully installed the isoorbi raw file reader version {isoraw_version}",
        start = start
      )
    }
  }

  # final check
  if (
    is.null(isoraw_version) || isoraw_version < numeric_version(min_version)
  ) {
    cli_abort(
      "cannot proceed, required isoorbi raw file reader (version {numeric_version(min_version)}) is missing or does not work"
    )
  }

  # license check
  check_license()
}

# get the path to the executable
get_isoraw_path <- function() {
  libdir <- tools::R_user_dir("isoorbi", which = "cache")
  d <- file.path(libdir, "assembly")
  if (Sys.info()["sysname"] == "Darwin") {
    f <- file.path(d, "isoraw-osx-x64")
  } else if (Sys.info()["sysname"] == "Linux") {
    f <- file.path(d, "isoraw-linux-x64")
  } else {
    f <- file.path(d, "isoraw-win-x64.exe")
  }
  return(f)
}

# check license acceptance
check_license <- function(env = caller_env()) {
  # path to license file
  license_file <- file.path(
    system.file(package = 'isoorbi'),
    'licenses',
    'RawFileReaderLicense.txt'
  )
  stopifnot(file.exists(license_file))

  # does the user copy already exist and is answered with YES?
  user_copy <- file.path(dirname(get_isoraw_path()), basename(license_file))
  question <- "Do you accept this license agreement for the use of the Thermo RawFileReader (https://github.com/thermofisherlsms/RawFileReader)?"
  if (
    file.exists(user_copy) && readLines(user_copy, 1) == paste(question, "YES")
  ) {
    # agreement is accepted --> can proceed
    return(invisible(TRUE))
  }

  # agreement is not yet accepted
  if (!interactive()) {
    # can't do it while knitting
    cli_abort(
      paste(
        "Please run this code interactively the first time so you can answer the prompt:",
        question
      ),
      call = env
    )
  }

  # show the liense and ask the question
  file.show(license_file)
  cli_text("{.strong {symbol$arrow_right} {question}}")
  response <- readline(prompt = "  Answer[Y/n]: ")
  if (tolower(response) %in% c("y", "yes")) {
    if (!dir.exists(dirname(user_copy))) {
      dir.create(dirname(user_copy), recursive = TRUE)
    }
    # user answered yes --> save
    writeLines(
      c(paste(question, "YES"), readLines(license_file)),
      user_copy
    )
    # double check
    if (
      file.exists(user_copy) &&
        readLines(user_copy, 1) == paste(question, "YES")
    ) {
      return(invisible(TRUE))
    }
    cli_abort(
      "Something went wrong saving your answer to {.file {user_copy}}, please try again.",
      call = env
    )
  }

  # not accepted, can't proceed
  cli_abort(
    "You have to accept the Thermo License agreement before using the RawFileReader.",
    call = env
  )
}


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

# read raw files =======

#' Read RAW files
#'
#' Read raw data files (`.raw`) from Orbitrap IRMS runs directly. This function extracts all available information and thus can be relatively slow (~1s / Mb on a typical personal computer) but with the caching this is only true the first time. The results can be used directly or, more typically, are aggregated with [orbi_aggregate_raw()] to safely extract the relevant information for downstream processing.  This function is designed to be fail save by safely catching errors and reporting back on them (see [orbi_get_problems()]).
#'
#' @param file_paths paths to the `.raw` file(s), single value or vector of paths. Use [orbi_find_raw()] to get all raw files in a folder.
#' @param show_progress whether to show a progress bar, by default always enabled when running interactively e.g. inside RStudio (and disabled in a notebook), turn off with `show_progress = FALSE`
#' @param show_problems whether to show problems encountered along the way (rather than just keeping track of them with [orbi_get_problems()]). Set to `show_problems = FALSE` to turn off the live printout. Either way, all encountered problems can be retrieved with running [orbi_get_problems()] for the returned list
#' @param include_spectra whether to include the spectral data from specific scans (e.g. `include_spectra = c(5, 100, 200)` reads out the spectra from scans 5, 100, and 200 for each file if they exist) or from all scans (`include_spectra = TRUE`). Including many or all scan spectra makes the read process slower (especially if `cache_spectra = FALSE`) and the returned data frame tibble significantely larger. The default is `FALSE` (i.e. scan spectra are not returned).
#' @param cache whether to automatically cache the read raw files (writes highly efficient .parquet files in a folder with the same name as the file .cache appended)
#' @param cache_spectra whether to automatically cache requested scan spectra (this can take up significant disc space), by default the same as `cache`
#' @param read_cache whether to read the file from cached .parquet files (if they exist) or anew
#' @return a tibble data frame where each row holds the file path and nested tibbles of datasets extracted from the raw file (typically `cache_info`, `scans`, `peaks`, and `spectra`). This is the safest way to extract the data without needing to make assumptions about compatibility across files. Extract your data of interest from the tibble columns or use [orbi_aggregate_raw()] to extract safely across files.
#' @export
orbi_read_raw <- function(
  file_paths,
  show_progress = rlang::is_interactive(),
  show_problems = TRUE,
  include_spectra = FALSE,
  cache = TRUE,
  cache_spectra = cache,
  read_cache = TRUE
) {
  # keep track of current env to anchor progress bars
  root_env <- current_env()

  # check for raw file reader
  orbi_check_isoraw()

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

  # read files safetly
  read_safely <- function(info, func, ...) {
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

    # start timer
    file_start <- start_info()

    # function (so traceback is informative)
    func_quo <- expr(
      (!!func)(
        cache_info = info,
        all_spectra = all_spectra,
        select_spectra = select_spectra,
        cache = cache,
        cache_spectra = cache_spectra,
        pb = start$pb,
        .env = root_env
      )
    )

    # call with error handling
    out <-
      try_catch_cnds(
        eval_tidy(func_quo),
        error_value = tibble(
          file_path = info$file_path,
          problems = list(tibble())
        ),
        catch_errors = !orbi_get_option("debug")
      )

    # did we get anything back?
    has_file_info <- "file_info" %in% names(out$result)

    # how many scans?
    n_spectra_scans <- 0
    if ("spectra" %in% names(out$result) && length(out$result$spectra) > 0) {
      n_spectra_scans <- out$result$spectra[[1]]$scan |> unique() |> length()
    }

    # merge new into the returned problems
    problems <- out$conditions
    if ("problems" %in% names(out$result)) {
      problems <- bind_rows(problems, out$result$problems)
      out$result$problems <- list(problems)
    }

    # info
    finish_info(
      format_inline(...),
      start = file_start,
      conditions = problems,
      show_conditions = show_problems,
      .call = expr(orbi_read_raw()),
      .env = root_env
    )

    # add index and whether there is file info to result
    return(
      out$result |>
        dplyr::mutate(idx = info$idx, has_file_info = !!has_file_info)
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
    results <- cached_files |>
      split(1:nrow(cached_files)) |>
      purrr::map(
        read_safely,
        func = "read_cached_raw_file",
        "{if(!has_file_info) 'tried to '}",
        "read {.file {basename(info$file_path)}} from cache",
        "{if(n_spectra_scans > 0) format_inline(', included the {.field spectr{?um/a}} from {n_spectra_scans} {.field scan{?s}}')}"
      ) |>
      dplyr::bind_rows()
    finish_info(start = start)

    # stored cached data and figure out which files to (re)read
    cached_data <- results |> filter(.data$has_file_info)
    read_files <- cache_info |>
      dplyr::left_join(
        results |> dplyr::select("idx", "has_file_info"),
        by = "idx"
      ) |>
      dplyr::filter(is.na(.data$has_file_info) | !.data$has_file_info)
  } else {
    # nothing cached, read all
    cached_data <- tibble()
    read_files <- cache_info
  }

  # any files to read?
  if (nrow(read_files) > 0) {
    read_files <- read_files |>
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
    results <- read_files |>
      split(1:nrow(read_files)) |>
      purrr::map(
        read_safely,
        func = "read_raw_file",
        "{if(info$is_cached) 're-read' else 'read'} ",
        "{.file {basename(info$file_path)}} ",
        "({if (info$file_exists) prettyunits::pretty_bytes(info$file_size) else '? B'})",
        "{if(n_spectra_scans > 0) format_inline(', included the {.field spectr{?um/a}} from {n_spectra_scans} {.field scan{?s}}')}"
      ) |>
      dplyr::bind_rows()
    finish_info(start = start)
  } else {
    results <- tibble()
  }

  # get all data
  all_files <-
    cached_data |>
    dplyr::bind_rows(results) |>
    dplyr::arrange(.data$idx) |>
    dplyr::select(-dplyr::any_of(c("idx", "has_file_info")))

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
      cache_info$result |>
        # backwards compatibility
        dplyr::mutate(isoorbi_version = as.character(.data$isoorbi_version)) |>
        set_names(paste0("old_", names(cache_info$result)))
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
    tidyr::unnest("info", keep_empty = TRUE) |>
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
  # add condition column (since it is never cached)
  problems <- problems$result |> dplyr::mutate(condition = list(list()))

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
      if (cache_info$file_exists) {
        # try to read missing spectra
        update_progress("reading spectra")
        spectra_raw <- try_catch_cnds(
          rawrr::readSpectrum(
            cache_info$file_path,
            scan = missing_spectra
          ),
          error_value = list(),
          catch_errors = !orbi_get_option("debug")
        )
        problems <- bind_rows(problems, spectra_raw$conditions)

        # try to parse missing spectra
        update_progress("parsing spectra")
        spectra_parsed <- try_catch_cnds(
          spectra_raw$result |>
            parse_spectra() |>
            parse_spectral_data(),
          error_value = tibble(scan = integer(0), mZ = numeric(0)),
          catch_errors = !orbi_get_option("debug")
        )
        problems <- bind_rows(problems, spectra_parsed$conditions)

        # still missing?
        missing_spectra <- setdiff(missing_spectra, spectra_parsed$result$scan)
        if (length(missing_spectra) > 0) {
          # can't get everything
          cli_warn(
            "missing {length(missing_spectra)} spectr{?um/a} that {?is/are} not cached and could not be read from the .raw file"
          )
        }

        # combine if there are new spectra
        if (nrow(spectra_parsed$result) > 0) {
          spectral_data$result <- spectral_data$result |>
            dplyr::bind_rows(spectra_parsed$result) |>
            dplyr::arrange(.data$scan, .data$mZ)

          # caching
          if (cache && cache_spectra) {
            arrow::write_parquet(
              spectral_data$result,
              sink = cache_info$spectra
            )
          }
        }
      } else {
        cli_warn(
          "missing {length(missing_spectra)} spectr{?um/a} that {?is/are} not cached (.raw file is not available)"
        )
      }
    }
    # subset what we got if we don't want all
    if (!all_spectra) {
      spectral_data$result <- spectral_data$result |>
        dplyr::filter(.data$scan %in% !!select_spectra)
    }
  }

  # wrapping up
  update_progress("finalizing")
  problems <- problems |>
    dplyr::filter(
      .by = c("type", "call", "message"),
      dplyr::n() == 1L | !purrr::map_lgl(.data$condition, is_empty)
    )

  # cache problems if there are new ones (e.g. from additional spectral read)
  if (cache && any(!purrr::map_lgl(problems$condition, is_empty))) {
    problems |>
      # CANNOT cache the actual error ojects
      dplyr::select(-"condition") |>
      arrow::write_parquet(sink = cache_info$problems)
  }

  # return
  tibble(
    file_path = cache_info$file_path,
    file_info = list(file_info$result),
    scans = list(scans$result),
    peaks = list(peaks$result),
    spectra = list(spectral_data$result),
    problems = list(problems)
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
  cache_spectra = cache,
  pb = NULL,
  .env = caller_env()
) {
  # safety checks
  if (!cache_info$file_exists) {
    cli_abort("cannot find this .raw file")
  }

  # simplify progress updates
  update_progress <- function(status) {
    if (!is.null(pb)) {
      cli_progress_update(
        id = pb,
        inc = 0,
        status = status,
        .envir = .env,
        force = TRUE
      )
    }
  }

  # start problems
  problems <- tibble()

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

  ## read indices
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

  ## read spectra/scans (always needed to get all scan info even if spectra not returned)
  update_progress("reading scans")
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
  update_progress("parsing spectra")
  spectra_parsed <- try_catch_cnds(
    parse_spectra(spectra_raw$result),
    error_value = tibble(scan = integer(0)),
    catch_errors = !orbi_get_option("debug")
  )
  problems <- bind_rows(problems, spectra_parsed$conditions)

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

  # decide what spectra to return
  spectra <- tibble(scan = integer(0))
  if (all_spectra || length(select_spectra) > 0) {
    update_progress("selecting spectra")
    spectral_data <- try_catch_cnds(
      spectra_parsed$result |>
        # everything OR the requested ones
        dplyr::filter(!!all_spectra | .data$scan %in% !!select_spectra) |>
        parse_spectral_data(),
      error_value = tibble(scan = integer(0)),
      catch_errors = !orbi_get_option("debug")
    )
    problems <- bind_rows(problems, spectral_data$conditions)
    spectra <- spectral_data$result
  }

  #Sys.sleep(0.5) # FIXME
  # if (grepl("_10scans", cache_info$file_path)) {
  #   cli_abort("something totally wrong")
  # } # FIXME

  ## wrapping up
  update_progress("finalizing")
  problems <- problems |>
    dplyr::filter(
      .by = c("type", "call", "message"),
      dplyr::n() == 1L | !purrr::map_lgl(.data$condition, is_empty)
    )

  ## caching
  if (cache) {
    # do we need to create the cache path?
    if (!dir.exists(cache_info$cache_path)) {
      dir.create(cache_info$cache_path)
    }

    # cache info
    arrow::write_parquet(
      tibble(
        file_size = as.integer(cache_info$file_size),
        isoorbi_version = as.character(utils::packageVersion("isoorbi")),
        cache_timestamp = Sys.time()
      ),
      sink = cache_info$cache_info
    )

    # file info
    arrow::write_parquet(file_info, sink = cache_info$file_info)

    # scan info
    arrow::write_parquet(scans, sink = cache_info$scans)

    # cache peaks
    arrow::write_parquet(peaks, sink = cache_info$peaks)

    # spectra
    if (cache_spectra) {
      arrow::write_parquet(spectra, sink = cache_info$spectra)
    }

    # problems
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

# here because it's used in both read_cached_raw_file and read_raw_file
parse_spectra <- function(spectra_raw) {
  spectra_raw |>
    map(
      function(spectrum) {
        if (!"scan" %in% names(spectrum)) {
          cli_abort("no {.var scan} column found in spectrum")
        }
        spectrum$scan <- as.integer(spectrum$scan)
        convert_list_to_tibble(spectrum)
      }
    ) |>
    dplyr::bind_rows()
}

# here because it's used in both read_cached_raw_file and read_raw_file
parse_spectral_data <- function(spectra_parsed) {
  data_cols <- grepl("^(mZ|intensity)", names(spectra_parsed))
  if (!any(data_cols)) {
    cli_abort(
      "no raw data ({.var mZ} & {.var intensity}) found in spectra, returning empty raw data table"
    )
  }
  spectra_parsed[names(spectra_parsed) == "scan" | data_cols] |>
    tibble::as_tibble() |>
    tidyr::unnest(-"scan")
}

# utility function ==========

# turns multi value entries into list columns and omits NULL values
convert_list_to_tibble <- function(cache_info_list) {
  lens <- lengths(cache_info_list)
  cache_info_list[lens > 1] <- lapply(cache_info_list[lens > 1], list)
  return(tibble::as_tibble(cache_info_list[lens > 0]))
}
