# isoraw installation ========

#' Check for the isoorbi raw file reader
#'
#' By default, this will install the isoraw reader if it is missing or outdated, and will ask the user to agree to Thermo's license agreement
#' for the \href{https://github.com/thermofisherlsms/RawFileReader}{Thermo RawFileReader} before proceeding.
#' This function runs automatically during a raw file read and does not usually need to be called directly by the user.
#'
#' @param install_if_missing install the reader if it's missing
#' @param reinstall_if_outdated install the reader if it's outdated (i.e. not at least `min_version`)
#' @param reinstall_always whether to (re-)install no matter what
#' @param min_version the minimum version number required
#' @param source the URL (or local path) where to find the raw file reader, by default this is the latests release of the executables on github
#' @param ... passed on to `download.file` if (re-) installing the reader
#' @export
orbi_check_isoraw <- function(
  install_if_missing = TRUE,
  reinstall_if_outdated = TRUE,
  reinstall_always = FALSE,
  min_version = "0.2.1",
  source = paste0(
    "https://github.com/isoverse/isoorbi/releases/download/isoraw-v",
    min_version
  ),
  ...
) {
  # start
  start <- start_info()

  # check existence
  isoraw_exists <- file.exists(get_isoraw_path())
  outdated <- FALSE

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
        tmpfile <- tempfile()
        if (dir.exists(source)) {
          # local folder (usually only used by developers)
          file.copy(file.path(source, basename(get_isoraw_path())), tmpfile)
        } else {
          # download to temp file
          download.file(
            file.path(source, basename(get_isoraw_path())),
            destfile = tmpfile,
            mode = "wb",
            ...
          )
        }
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

# interactions with isoraw =======

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

# get version of the installed isoraw
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

# run the isoraw executable
# @return the cache path where the files are located
run_isoraw <- function(
  path,
  skip_file_info = FALSE,
  skip_scans = FALSE,
  skip_peaks = FALSE,
  all_spectra = FALSE,
  select_spectra = integer(0)
) {
  # safety checks
  stopifnot(
    is_scalar_character(path),
    is_scalar_logical(skip_file_info),
    is_scalar_logical(skip_scans),
    is_scalar_logical(skip_peaks),
    is_scalar_logical(all_spectra),
    is_integerish(select_spectra)
  )
  select_spectra <- as.integer(select_spectra) |> na.omit()

  # assemble arguments
  args <- c("--file", sprintf("\"%s\"", path))
  if (skip_file_info || skip_scans || skip_peaks) {
    args <- c(
      args,
      "--skip",
      paste(
        c(
          if (skip_file_info) "fileInfo",
          if (skip_scans) "scans",
          if (skip_peaks) "peaks"
        ),
        collapse = ","
      )
    )
  }
  if (all_spectra || length(select_spectra) > 0) {
    args <- c(
      args,
      "--spectra",
      if (all_spectra) "all" else paste(select_spectra, collapse = ",")
    )
  }

  # ran the executable
  output <- system2(
    get_isoraw_path(),
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )

  # create the log file (if inital file exists)
  logfile <- NULL
  output_path <- paste0(path, ".cache")
  output_exists <- dir.exists(output_path)
  if (file.exists(path)) {
    if (!output_exists) {
      dir.create(output_path, recursive = TRUE)
    }
    logfile <- file.path(output_path, "isoraw.log")
    write(
      c(
        "\n==================",
        sprintf(
          "isoraw call on %s\n%s %s",
          as.character(Sys.time()),
          basename(get_isoraw_path()),
          paste(args, collapse = " ")
        ),
        output
      ),
      file = logfile,
      append = TRUE,
      sep = "\n"
    )
  }

  # check for errors
  errors <- output[grepl("error", output, ignore.case = TRUE)]
  if (length(errors) > 0) {
    cli_abort(
      c(
        "encountered {length(errors)} error{?s} when running the isoraw raw file reader",
        # having the log file makes it possible for users to click on the link and see exactly what's going wrong
        if (!is.null(logfile)) c("i" = "see {.file {logfile}} for details"),
        errors |> set_names("x")
      )
    )
  }

  # check if cache path was created by the program
  if (!output_exists) {
    cli_abort(
      c(
        "encountered unknown issue when running the isoraw raw file reader, no output was created",
        if (!is.null(logfile)) c("i" = "see {.file {logfile}} for details")
      )
    )
  }

  # return the path where the output files are located
  return(output_path)
}

# read an isoraw output file
read_isoraw_output <- function(path) {
  if (!file.exists(path)) {
    cli_abort("isoraw output file {.file {basename(path)}} is missing")
  }
  tryCatch(
    arrow::read_parquet(path),
    error = function(cnd) {
      cli_abort(
        "could not read isoraw output file {.file {basename(path)}}",
        parent = cnd
      )
    }
  )
}

# raw file caching ======

# cache the isoraw output by storing it in a zip file
cache_isoraw_output <- function(
  output_path,
  zip_path = paste0(output_path, ".zip")
) {
  if (file.exists(zip_path)) {
    unlink(zip_path)
  }
  old <- setwd(dir = dirname(output_path))
  on.exit(setwd(old))
  files <- c(
    basename(output_path),
    list.files(basename(output_path), full.names = TRUE)
  )
  utils::zip(
    zipfile = basename(zip_path),
    files = files,
    flags = "-q" # quiet
  )
}

# read isoraw output file from zipped cache
read_cached_isoraw_output <- function(
  zip_path,
  subfile,
  zip_info = unzip(zip_path, list = TRUE)
) {
  stopifnot(subfile %in% zip_info$Name)
  file_size <- zip_info$Length[zip_info$Name == subfile]
  con <- unz(zip_path, subfile, open = "rb")
  parquet_output <-
    tryCatch(
      con |>
        readBin("raw", n = file_size) |>
        arrow::read_parquet(),
      error = function(cnd) {
        cli_abort(
          "could not read cached isoraw output file {.file {basename(subfile)}}",
          parent = cnd
        )
      }
    )
  close(con)
  return(parquet_output)
}

# finding raw files ========

#' Find raw files
#' @description Finds all .raw files in a folder.
#' @param folder path to a folder with raw files
#' @param include_cache whether to include .raw.cache.zip folders in the absence of the corresponding .raw file so that copies of the cache are read even in the absence of the original raw files
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
    cache_files <- list.files(
      folder,
      pattern = "\\.raw\\.cache\\.zip$",
      full.names = TRUE,
      recursive = recursive
    ) |>
      unique()
    if (length(cache_files) > 0) {
      # include folders
      linked_files <- gsub("\\.raw\\.cache\\.zip$", ".raw", cache_files)
      cache_files <- cache_files[!linked_files %in% files]
      files <- c(files, cache_files) |> unique() |> sort()
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
#' @param read_cache whether to read the file from cached .parquet files (if they exist) or anew
#' @param cache whether to automatically cache the read raw files (writes highly efficient .parquet files in a folder with the same name as the file .cache appended)
#' @param cache_spectra whether to automatically cache requested scan spectra (this can take up significant disc space), by default the same as `cache`
#' @param keep_chached_spectra whether to keep the spectra from a raw file that were previously cached whenever `include_spectra` changes and requires reading the file anew. Having this TRUE (the default) makes it faster to iterate on code that changes which spectra to read but leads to larger cache files.
#' @return a tibble data frame where each row holds the file path and nested tibbles of datasets extracted from the raw file (typically `file_info`, `scans`, `peaks`, and `spectra`). This is the safest way to extract the data without needing to make assumptions about compatibility across files. Extract your data of interest from the tibble columns or use [orbi_aggregate_raw()] to extract safely across files.
#' @export
orbi_read_raw <- function(
  file_paths,
  show_progress = rlang::is_interactive(),
  show_problems = TRUE,
  include_spectra = FALSE,
  read_cache = TRUE,
  cache = TRUE,
  cache_spectra = cache,
  keep_cached_spectra = cache
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
    select_spectra <- integer()
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

  # generate caching info
  file_paths_info <- get_file_paths_info(
    file_paths = unique(file_paths),
    read_cache = read_cache
  )

  # read files safely
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
        file_path_info = info,
        all_spectra = all_spectra,
        select_spectra = select_spectra,
        cache = cache,
        cache_spectra = cache_spectra,
        keep_cached_spectra = keep_cached_spectra,
        pb = start$pb,
        .env = root_env
      )
    )

    # call with error handling
    out <-
      try_catch_cnds(
        eval_tidy(func_quo),
        error_value = tibble(
          filepath = info$file_path,
          problems = list(tibble())
        ),
        catch_errors = !orbi_get_option("debug")
      )

    # did we get anything back?
    has_file_info <- "file_info" %in% names(out$result)

    # how many scans?
    n_spectra_scans <- 0
    if ("spectra" %in% names(out$result) && length(out$result$spectra) > 0) {
      n_spectra_scans <- out$result$spectra[[1]]$scan.no |> unique() |> length()
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

    # add index and whether there is file info in the result
    return(
      out$result |>
        dplyr::mutate(idx = info$idx, has_file_info = !!has_file_info)
    )
  }

  # read cached files first
  if (any(file_paths_info$is_cached)) {
    # subset files to read just the cached ones
    cached_files <- file_paths_info |>
      dplyr::filter(.data$is_cached) |>
      dplyr::mutate(
        # progress bar info
        step = cumsum(dplyr::lag(.data$cache_size, default = 0)),
        total = sum(.data$cache_size),
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
    read_files <- file_paths_info |>
      dplyr::left_join(
        results |> dplyr::select("idx", "has_file_info"),
        by = "idx"
      ) |>
      dplyr::filter(is.na(.data$has_file_info) | !.data$has_file_info)
  } else {
    # nothing cached, read all
    cached_data <- tibble()
    read_files <- file_paths_info
  }

  # any files to read?
  if (nrow(read_files) > 0) {
    read_files <- read_files |>
      dplyr::mutate(
        # progress bar info
        step = cumsum(dplyr::lag(.data$file_size, default = 0)),
        total = sum(.data$file_size)
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
  class(all_files) <- unique(c("orbi_raw_files", class(all_files)))
  return(all_files)
}

#' @export
print.orbi_raw_files <- function(x, ...) {
  cli_rule(
    center = "{.strong {length(x$filepath)} raw file{?s} - {?process/combine} {?it/them} with orbi_aggregate_raw()}"
  )

  n_digits <- function(x) {
    ifelse(x == 0, 1, floor(log10(abs(x))) + 1)
  }

  x |>
    dplyr::mutate(
      idx = dplyr::row_number(),
      idx_spacers = max(n_digits(idx)) - n_digits(idx),
      filename_spacers = max(nchar(basename(filepath))) -
        nchar(basename(filepath)),
      n_scans = purrr::map_int(.data$scans, nrow),
      scans_spacers = max(n_digits(n_scans)) - n_digits(n_scans),
      n_peaks = purrr::map_int(.data$peaks, nrow),
      peaks_spacers = max(n_digits(n_peaks)) - n_digits(n_peaks),
      n_spectral_data = purrr::map_int(.data$spectra, nrow),
      n_spectra = purrr::map_int(.data$spectra, ~ length(unique(.x$scan.no))),
      n_problems = purrr::map_int(.data$problems, nrow),
      problems_text = purrr::map_chr(
        .data$problems,
        summarize_cnds,
        include_symbol = FALSE,
        include_call = FALSE
      )
    ) |>
    dplyr::mutate(
      .by = "idx",
      # format_inline needs a single line
      label = paste0(
        strrep("\u00a0", idx_spacers),
        format_inline("{idx}. {col_blue(basename(filepath))} "),
        strrep("\u00a0", filename_spacers),
        if_else(
          .data$n_problems > 0,
          format_inline("encountered {problems_text}; has "),
          "has "
        ),
        strrep("\u00a0", scans_spacers),
        format_inline(
          "{n_scans} {.field scans} with "
        ),
        strrep("\u00a0", peaks_spacers),
        format_inline(
          "{n_peaks} {.field peak{?s}}; ",
          if_else(
            .data$n_spectral_data > 0,
            "+ loaded {n_spectra} {.field spectr{?um/a}} ({n_spectral_data} points)",
            "no {.field spectra} were loaded"
          )
        )
      )
    ) |>
    dplyr::pull(label) |>
    cli_bullets_raw()
}

# copmile information about the file paths
get_file_paths_info <- function(
  file_paths,
  read_cache = TRUE
) {
  tibble(
    idx = seq_along(!!file_paths),
    file_path = gsub("\\.cache\\.zip$", "", !!file_paths),
    file_exists = file.exists(.data$file_path),
    file_size = as.integer(file.size(.data$file_path)),
    output_path = paste0(.data$file_path, ".cache"),
    cache_path = paste0(.data$output_path, ".zip"),
    cache_size = as.integer(file.size(.data$cache_path)),
    cache_info = "cache_info.parquet",
    file_info = "file_info.parquet",
    scans = "scans.parquet",
    peaks = "peaks.parquet",
    problems = "problems.parquet",
    spectra = "spectra.parquet",
    is_cached = !!read_cache & file.exists(.data$cache_path),
    read_file_size = if_else(
      .data$is_cached,
      as.integer(file.size(.data$cache_path)),
      .data$file_size
    )
  )
}

# reader for cached raw files
# @param file_path can be used for a direct call (usually for testing only)
# @param file_path_info typically called with this parameter internally
read_cached_raw_file <- function(
  file_path_info = get_file_paths_info(
    file_path,
    read_cache = read_cache
  ),
  file_path = NULL,
  all_spectra = FALSE,
  select_spectra = integer(0),
  read_cache = TRUE,
  cache = TRUE,
  cache_spectra = TRUE,
  keep_cached_spectra = TRUE,
  pb = NULL,
  .env = caller_env()
) {
  # safety checks
  check_arg(
    file_path_info,
    is.data.frame(file_path_info) && nrow(file_path_info) == 1,
    "must be a single line data frame"
  )
  if (!file_path_info$is_cached || !file.exists(file_path_info$cache_path)) {
    cli_abort(
      "there's no cache to read for {.file {basename(file_path_info$file_path)}}",
      .internal = TRUE
    )
  }

  # check if cached files exist
  zip_info <- unzip(file_path_info$cache_path, list = TRUE)
  cache_path_subpaths <- file.path(
    basename(file_path_info$output_path),
    c(
      file_path_info$cache_info,
      file_path_info$file_info,
      file_path_info$scans,
      file_path_info$peaks,
      file_path_info$problems
    )
  )
  missing_subpaths <- !cache_path_subpaths %in% zip_info$Name
  if (any(missing_subpaths)) {
    cli_abort(
      "cached archive is missing file{?s}: {.file {basename(cache_path_subpaths[missing_subpaths])}}"
    )
  }

  # read cache info
  read_cached_output <- function(file) {
    read_cached_isoraw_output(
      zip_path = file_path_info$cache_path,
      subfile = file_path_info$output_path |>
        basename() |>
        file.path(file),
      zip_info = zip_info
    )
  }
  existing_cache_info <- try_catch_cnds(read_cached_output(
    file_path_info$cache_info
  ))
  if (nrow(existing_cache_info$conditions) > 0) {
    cli_abort("failed to read cache info")
  }

  # check about file size
  if (
    !is.na(file_path_info$file_size) &&
      file_path_info$file_size != existing_cache_info$result$file_size
  ) {
    cli_warn(
      "file size has changed (from {prettyunits::pretty_bytes(existing_cache_info$result$file_size)} to {prettyunits::pretty_bytes(file_path_info$file_size)}), cache is outdated"
    )
    return(tibble())
  }

  # check about isoorbi version
  # can use existing_cache_info$isoorbi_version to determine whether a new read is necessary

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
  file_info <- try_catch_cnds(read_cached_output(file_path_info$file_info))
  if (nrow(file_info$conditions) > 0) {
    cli_abort("failed to read cached file info")
  }

  # read scans from cache
  update_progress("reading cached scans")
  scans <- try_catch_cnds(read_cached_output(file_path_info$scans))
  if (nrow(scans$conditions) > 0) {
    cli_abort("failed to read cached scans")
  }

  # read peaks from cache
  update_progress("reading cached peaks")
  peaks <- try_catch_cnds(read_cached_output(file_path_info$peaks))
  if (nrow(peaks$conditions) > 0) {
    cli_abort("failed to read cached peaks")
  }

  # read problems from cache
  update_progress("reading cached problems")
  problems <- try_catch_cnds(read_cached_output(file_path_info$problems))
  if (nrow(problems$conditions) > 0) {
    cli_abort("failed to read cached problems")
  }
  # add condition column (since it is never cached)
  problems <- problems$result |> dplyr::mutate(condition = list(list()))

  # read spectra from cache
  update_progress("reading cached spectra")
  spectra <- list(
    result = tibble(scan.no = integer()),
    conditions = tibble()
  )

  # are any spectra actually requested?
  if (all_spectra || length(select_spectra) > 0) {
    # read what's in the cache
    if (
      file.path(
        basename(file_path_info$output_path),
        file_path_info$spectra
      ) %in%
        zip_info$Name
    ) {
      spectra <- try_catch_cnds(read_cached_output(
        file_path_info$spectra
      ))
      if (nrow(spectra$conditions) > 0) {
        cli_abort("failed to read cached spectra")
      }
    }

    # do we have missing spectra?
    missing_spectra <-
      if (all_spectra) {
        scans$result$scan.no |> setdiff(spectra$result$scan.no)
      } else {
        select_spectra |>
          intersect(scans$result$scan.no) |>
          setdiff(spectra$result$scan.no)
      }

    # yes --> let's see if we can get the missing spectra
    if (length(missing_spectra) > 0) {
      if (file_path_info$file_exists) {
        # try to read missing spectra
        update_progress("running isoraw")

        # do we keep the cached spectra?
        if (!all_spectra && keep_cached_spectra) {
          select_spectra <- select_spectra |>
            union(spectra$result$scan.no)
        }

        # unzip the cache file
        unlink(file_path_info$output_path, recursive = TRUE)
        unzip(
          file_path_info$cache_path,
          exdir = dirname(file_path_info$output_path)
        )

        # run isoraw only for the spectra
        out <- try_catch_cnds(
          run_isoraw(
            file_path_info$file_path,
            skip_file_info = TRUE,
            skip_scans = TRUE,
            skip_peaks = TRUE,
            all_spectra = all_spectra,
            select_spectra = select_spectra
          ),
          error_value = NULL,
          catch_errors = !orbi_get_option("debug")
        )
        problems <- dplyr::bind_rows(problems, out$conditions)

        update_progress("reading spectra")
        spectra <- try_catch_cnds(
          read_isoraw_output(file.path(
            file_path_info$output_path,
            file_path_info$spectra
          )),
          error_value = tibble(scan.no = integer(0))
        )
        problems <- dplyr::bind_rows(problems, spectra$conditions)

        # still missing?
        missing_spectra <- setdiff(missing_spectra, spectra$result$scan.no)
        if (length(missing_spectra) > 0) {
          # can't get everything
          cli_warn(
            "missing {length(missing_spectra)} spectr{?um/a} that {?is/are} not cached and could not be read from the .raw file"
          )
        } else if (cache && cache_spectra) {
          # none missing and we're caching --> zip up the cache files
          out <- try_catch_cnds(cache_isoraw_output(
            file_path_info$output_path,
            file_path_info$cache_path
          ))
          if (nrow(out$conditions) > 0) {
            show_cnds(
              out$conditions,
              message = "encountered these issues while generating cache file {file_path_info$cache_path}"
            )
          }
        }

        # cleanup
        if (dir.exists(file_path_info$output_path)) {
          unlink(file_path_info$output_path, recursive = TRUE)
        }
      } else {
        cli_warn(
          "missing {length(missing_spectra)} spectr{?um/a} that {?is/are} not cached (.raw file is not available)"
        )
      }
    }
    # subset what we got if we don't want all
    if (!all_spectra) {
      spectra$result <- spectra$result |>
        dplyr::filter(.data$scan.no %in% !!select_spectra)
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
      arrow::write_parquet(sink = file_path_info$problems)
  }

  # return
  tibble(
    # filepath instead of file_path for consistency with isox
    filepath = file_path_info$file_path,
    file_info = list(file_info$result),
    scans = list(scans$result),
    peaks = list(peaks$result),
    spectra = list(spectra$result),
    problems = list(problems)
  )
}

# raw file reader
# @param file_path can be used for a direct call (usually for testing only)
# @param file_path_info typically called with this parameter internally
read_raw_file <- function(
  file_path_info = get_file_paths_info(
    file_path,
    read_cache = read_cache
  ),
  file_path = NULL,
  all_spectra = FALSE,
  select_spectra = integer(0),
  read_cache = TRUE,
  cache = TRUE,
  cache_spectra = cache,
  pb = NULL,
  .env = caller_env(),
  ...
) {
  # safety checks
  if (!file_path_info$file_exists) {
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

  # clear output path
  if (dir.exists(file_path_info$output_path)) {
    unlink(file_path_info$output_path, recursive = TRUE)
  }

  # run isoraw
  update_progress("running isoraw")
  out <- try_catch_cnds(
    run_isoraw(
      file_path_info$file_path,
      all_spectra = all_spectra,
      select_spectra = select_spectra
    ),
    error_value = NULL,
    catch_errors = !orbi_get_option("debug")
  )
  problems <- out$conditions

  # read file_info from output
  update_progress("reading file info")
  file_info <- try_catch_cnds(
    read_isoraw_output(
      file.path(
        file_path_info$output_path,
        file_path_info$file_info
      )
    ),
    error_value = tibble()
  )
  problems <- dplyr::bind_rows(problems, file_info$conditions)

  # read scans from output
  update_progress("reading scans")
  scans <- try_catch_cnds(
    read_isoraw_output(file.path(
      file_path_info$output_path,
      file_path_info$scans
    )),
    error_value = tibble(scan.no = integer(0))
  )
  problems <- dplyr::bind_rows(problems, scans$conditions)

  # read peaks from output
  update_progress("reading peaks")
  peaks <- try_catch_cnds(
    read_isoraw_output(file.path(
      file_path_info$output_path,
      file_path_info$peaks
    )),
    error_value = tibble(scan.no = integer(0))
  )
  problems <- dplyr::bind_rows(problems, peaks$conditions)

  # read spectra from output
  if (all_spectra || length(select_spectra) > 0) {
    update_progress("reading spectra")
    spectra <- try_catch_cnds(
      read_isoraw_output(file.path(
        file_path_info$output_path,
        file_path_info$spectra
      )),
      error_value = tibble(scan.no = integer(0))
    )
    problems <- dplyr::bind_rows(problems, spectra$conditions)
  } else {
    spectra <- list(result = tibble(scan.no = integer(0)))
  }

  # wrapping up
  update_progress("finalizing")
  problems <- problems |>
    dplyr::filter(
      .by = c("type", "call", "message"),
      dplyr::n() == 1L | !purrr::map_lgl(.data$condition, is_empty)
    )

  # caching (only if at least the isoraw read had no issues)
  cleanup <- nrow(out$conditions) == 0L
  if (cache && cleanup) {
    # cache info
    arrow::write_parquet(
      tibble(
        file_size = as.integer(file_path_info$file_size),
        isoorbi_version = as.character(utils::packageVersion("isoorbi")),
        cache_timestamp = Sys.time()
      ),
      sink = file.path(file_path_info$output_path, file_path_info$cache_info)
    )

    # caching problems
    problems |>
      # CANNOT cache the actual error ojects
      dplyr::select(-"condition") |>
      arrow::write_parquet(
        sink = file.path(file_path_info$output_path, file_path_info$problems)
      )

    # zipping up the cache file
    out <- try_catch_cnds(cache_isoraw_output(
      file_path_info$output_path,
      file_path_info$cache_path
    ))
    if (nrow(out$conditions) > 0) {
      cleanup <- FALSE # stop cleanup, something went wrong
      show_cnds(
        out$conditions,
        message = "encountered these issues while generating cache file {file_path_info$cache_path}"
      )
    }
  }

  # cleanup
  if (cleanup) {
    unlink(file_path_info$output_path, recursive = TRUE)
  }

  # combine
  out <-
    tibble(
      file_path = file_path_info$file_path,
      file_info = list(file_info$result),
      scans = list(scans$result),
      peaks = list(peaks$result),
      spectra = list(spectra$result),
      problems = list(problems)
    )

  # return
  return(out)
}
