# read several raw files with progress bar and summary stats
orbi_read_raw <- function(file_paths, show_progress = rlang::is_interactive(), show_problems = TRUE) {
  
  # get started
  n_files <- length(file_paths)
  if (n_files == 0) cli_abort("no {.var file_paths} provided")
  start <- Sys.time()
  
  # progress bar
  pb <- NULL
  if (show_progress) { 
    old <- options(cli.progress_show_after = 0); on.exit(options(old))
    pb_multiplier <- 10 # for subprocesses
    pb <- cli_progress_bar(
      extra = list(file_path = file_paths[1], n_files = n_files, multiplier = pb_multiplier), 
      total = n_files * pb_multiplier,
      format =
        paste(
          "Reading {floor(pb_current/pb_extra$multiplier) + 1}/{pb_extra$n_files}",
          "raw files {pb_bar} {pb_percent}",
          "| {pb_elapsed} | ETA{pb_eta} | {.emph {basename(pb_extra$file_path)}}",
          "({prettyunits::pretty_bytes(file.size(pb_extra$file_path))})",
          "| step {pb_current %% pb_extra$multiplier + 1}: {.field {pb_status}}"
        )
    ); on.exit(cli_process_done(), add = TRUE)
  }
  
  # read files safely and with progress info
  read_safely_with_progress <- function(file_path, i) {
    
    # progress
    if (show_progress) 
      cli_progress_update(
        id = pb, set = (i - 1) * 10, 
        extra = list(file_path = file_path),
        status = "initializing"
      )
    
    # read file but catch errors in case there are any uncaught ones
    out <- 
      try_catch_cnds(
        read_raw_file(file_path, info = sprintf("%d/%d ", i, n_files),
                      show_problems = show_problems, 
                      show_progress = show_progress, pb = pb), 
        error_value = tibble(file_path = file_path, problems = list(tibble())),
        show_cnds = list(
          include_summary = list(
            message = sprintf("for {.emph %s}", basename(file_path)), call = FALSE
          )
        )
      )
    
    # merge new into the returned problems
    out$result$problems <- list(bind_rows(out$result$problems, out$conditions))
  
    # return
    return(out$result)
  }
  
  # read files (separate calls to simplify backtraces)
  results <- file_paths |> map2(seq_along(file_paths), read_safely_with_progress)
  results <- results |> bind_rows()

  # info
  problems <- results$problems |> dplyr::bind_rows()
  if (show_progress) cli_progress_done(id = pb)
  if (n_files > 1) {
    # if there's more than 1 file read
    info <- format_inline(
      "{col_green(symbol$tick)} ",
      "Read {n_files} raw file{?s} ", 
      "in {prettyunits::pretty_sec(as.numeric(Sys.time() - start, 'secs'))} ",
      summarize_cnds(
        problems, 
        "but encountered a total of {issues} {symbol$arrow_right} check with `orbi_get_problems(x)`")
    )
    cli_text(info)
  }
  
  # aggregation
  cli_text("Aggregating...")
  cli_alert_info("All done")
  
  # return
  return(results)
}

# raw file reader
# FIXME: should this really have it's own pb generation? probably not since it's a lower level function
read_raw_file <- function(file_path, show_progress = rlang::is_interactive(), show_problems = TRUE, info = NULL, pb = NULL) {
  
  # safety checks
  if (!file.exists(file_path)) rlang::abort("file does not exist")
  start <- Sys.time()
  
  # progress bar
  file_info <- format_inline(
    if (!is.null(info)) info,
    "{.emph {basename(file_path)}} ",
    "({prettyunits::pretty_bytes(file.size(file_path))})"
  )
  
  new_pb <- is.null(pb)
  if (show_progress && new_pb) { 
    old <- options(cli.progress_show_after = 0); on.exit(options(old))
    pb <- cli_progress_bar(
      paste("Reading", file_info),
      clear = FALSE, extra = list(problems = ""),
      format = "{pb_spin} {pb_name}{.field step {pb_current}: {pb_status}}| {pb_elapsed}"
    ); on.exit(cli_process_done(id = pb), add = TRUE)
    cli_progress_update(id = pb, status = "initializing")
  }
  
  # headers
  if (show_progress) cli_progress_update(id = pb, status = "read headers")
  headers <- try_catch_cnds(rawrr::readFileHeader(file_path), error_value = tibble())
  parse_headers <- function() convert_list_to_tibble(headers$result)
  headers_parsed <- try_catch_cnds(parse_headers(), error_value = tibble())
  
  # indices
  if (show_progress)  cli_progress_update(id = pb, status = "indices")
  indices <- try_catch_cnds(rawrr::readIndex(file_path), error_value = tibble(scan = integer(0)))
  parse_indices <- function() {
    if (!"scan" %in% names(indices$result)) rlang::abort("no {.var scan} column found in scan index")
    indices$result$scan <- as.integer(indices$result$scan)
    return(tibble::as_tibble(indices$result))
  }
  indices_parsed <- try_catch_cnds(parse_indices(), error_value =tibble(scan = integer(0)))
  
  # spectra
  if (show_progress) cli_progress_update(id = pb, status = "spectra")
  spectra <- try_catch_cnds(rawrr::readSpectrum(file_path, scan = indices$result$scan), error_value = list())
  
  # parse spectral data
  if (show_progress) cli_progress_update(id = pb, status = "parse spectra")
  parse_spectra <- function() {
    spectra <- spectra$result |> map(
      function(spectrum) {
        if (!"scan" %in% names(spectrum)) rlang::abort("no {.var scan} column found in spectrum")
        spectrum$scan <- as.integer(spectrum$scan)
        convert_list_to_tibble(spectrum)
      })
    dplyr::bind_rows(spectra)
  }
  spectra_parsed <- try_catch_cnds(parse_spectra(), error_value = tibble(scan = integer(0)))
  
  # scan info
  if (show_progress) cli_progress_update(id = pb, status = "parse scan info")
  parse_scan_info <- function() {
    scan_info_cols <- !str_detect(names(spectra_parsed$result), "^(centroid\\.|mZ|intensity)")
    scan_info <- spectra_parsed$result[, scan_info_cols]
    scan_cols <- setdiff(names(indices_parsed$result), names(scan_info))
    indices_parsed$result[c("scan", scan_cols)] |> 
      dplyr::left_join(scan_info, by = "scan")
  }
  scan_info <- try_catch_cnds(parse_scan_info(), error_value = tibble(scan = integer(0)))
  
  # peak table
  if (show_progress) cli_progress_update(id = pb, status = "parse peak table")
  parse_peak_table <- function() {
    peak_cols <- str_detect(names(spectra_parsed$result), "^centroid\\.")
    if (!any(peak_cols))
      rlang::abort("no centroid data found in spectra, returning empty peak table")
    peak_table <-
      spectra_parsed$result[names(spectra_parsed$result) == "scan" | peak_cols] |> 
      tibble::as_tibble()
    peak_table |> tidyr::unnest(-"scan")
  }
  peak_table <- try_catch_cnds(parse_peak_table(), error_value = tibble(scan = integer(0)))
  
  # raw data
  if (show_progress) cli_progress_update(id = pb, status = "parse raw data")
  parse_raw_data <- function() {
    data_cols <- str_detect(names(spectra_parsed$result), "^(mZ|intensity)")
    if (!any(data_cols))
      rlang::abort("no raw data ({.var mZ} & {.var intensity}) found in spectra, returning empty raw data table")
    raw_data <-
      spectra_parsed$result[names(spectra_parsed$result) == "scan" | data_cols] |> 
      tibble::as_tibble()
    raw_data |> tidyr::unnest(-"scan")
  }
  raw_data <- try_catch_cnds(parse_raw_data(), error_value = tibble(scan = integer(0)))
  
  # problems
  problems <- bind_rows(
    headers$conditions, headers_parsed$conditions, 
    indices$conditions, indices_parsed$conditions,
    spectra$conditions, spectra_parsed$conditions,
    scan_info$conditions, peak_table$conditions, raw_data$conditions)
  
  # info
  if (show_progress && new_pb) cli_process_done(id = pb)
  info <- format_inline(
    if (nrow(problems) == 0) "{col_green(symbol$tick)} " else "{col_green(symbol$info)} ",
    "{.timestamp {prettyunits::pretty_sec(as.numeric(Sys.time() - start, 'secs'))}} ",
    "Read {file_info} ", summarize_cnds(problems, "but encountered {issues}"),
    if (show_problems) ":"
  )
  cli_text(info)
  
  # problems info
  if (show_problems) {
    format_cnds(problems, prefix = "{symbol$arrow_right} ", indent = 1) |>
      cli_bullets()
  }
  
  # return
  tibble(
    file_path = file_path, 
    file_info = list(headers_parsed$result),
    scans = list(scan_info$result),
    peak_table = list(peak_table$result),
    raw_data = list(raw_data$result),
    problems = list(problems)
  )
}

# turns multi value entries into list columns and omits NULL values
convert_list_to_tibble <- function(file_info_list) {
  lens <- lengths(file_info_list)
  file_info_list[lens > 1] <- lapply(file_info_list[lens > 1], list)
  return(tibble::as_tibble(file_info_list[lens > 0]))
}
