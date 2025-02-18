# read the raw files =======

# read several raw files with progress bar and summary stats
orbi_read_raw <- function(file_paths, show_progress = rlang::is_interactive(), show_problems = TRUE) {
  
  # get started
  call <- force(rlang::current_call())
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
        id = pb, set = (i - 1) * 10, extra = list(file_path = file_path),
        status = "initializing"
      )
    
    # read file and catch errors
    file_start <- Sys.time()
    out <- 
      try_catch_cnds(
        read_raw_file(
          file_path, 
          show_problems = FALSE, # summarized below 
          show_progress = show_progress, pb = pb), 
        error_value = tibble(file_path = file_path, problems = list(tibble()))
      )
    
    # merge new into the returned problems
    problems <- bind_rows(out$result$problems, out$conditions)
    out$result$problems <- list(problems)
  
    # info
    info <- format_inline(
      if (nrow(problems) == 0) "{col_green(symbol$tick)} " else "{col_green(symbol$info)} ",
      "{.timestamp {prettyunits::pretty_sec(as.numeric(Sys.time() - file_start, 'secs'))}} ",
      "Read {.emph {basename(file_path)}} ",
      if (file.exists(file_path)) "({prettyunits::pretty_bytes(file.size(file_path))}) ",
      summarize_cnds(problems, "but encountered {issues}"),
      if (show_problems) ":"
    )
    cli_text(info)
    
    # problems info
    if (show_problems) show_cnds(problems, include_summary = FALSE, indent_cnds = TRUE)

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
  
  # return
  return(results)
}

# raw file reader
read_raw_file <- function(file_path, show_progress = rlang::is_interactive(), show_problems = TRUE, pb = NULL) {
  
  # safety checks
  if (!file.exists(file_path)) rlang::abort("file does not exist")
  
  # headers
  if (show_progress) cli_progress_update(id = pb, status = "read headers")
  headers <- try_catch_cnds(rawrr::readFileHeader(file_path), error_value = tibble())
  parse_headers <- function() convert_list_to_tibble(headers$result)
  headers_parsed <- try_catch_cnds(parse_headers(), error_value = tibble())
  
  # FIXME
  warning("bla")
  bla <- try_catch_cnds(test())
  
  # indices
  if (show_progress)  cli_progress_update(id = pb, status = "indices")
  indices <- try_catch_cnds(rawrr::readIndex(file_path), error_value = tibble(scan = integer(0)))
  parse_indices <- function() {
    if (!"scan" %in% names(indices$result)) rlang::abort("no {.var scan} column found in scan index")
    indices$result$scan <- as.integer(indices$result$scan)
    return(tibble::as_tibble(indices$result))
  }
  indices_parsed <- try_catch_cnds(parse_indices(), error_value = tibble(scan = integer(0)))
  
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
  
  # wrapping up
  if (show_progress) cli_progress_update(id = pb, status = "finalizing")
  
  # problems
  problems <- bind_rows(
    bla$conditions,
    headers$conditions, headers_parsed$conditions, 
    indices$conditions, indices_parsed$conditions,
    spectra$conditions, spectra_parsed$conditions,
    scan_info$conditions, peak_table$conditions, raw_data$conditions)
  if (show_problems) show_cnds(problems)
  
  # combine
  out <- 
    tibble(
      file_path = file_path, 
      file_info = list(headers_parsed$result),
      scans = list(scan_info$result),
      peak_table = list(peak_table$result),
      raw_data = list(raw_data$result),
      problems = list(problems)
    )
  
  # if there's a caching mechanism, it should go here
  
  # return
  return(out)
}

# turns multi value entries into list columns and omits NULL values
convert_list_to_tibble <- function(file_info_list) {
  lens <- lengths(file_info_list)
  file_info_list[lens > 1] <- lapply(file_info_list[lens > 1], list)
  return(tibble::as_tibble(file_info_list[lens > 0]))
}

# aggregate raw file data ========

#' aggregate data from raw files
#' @param files_data the files read in by orbi_read_raw
orbi_aggregate_raw <- function(files_data, aggregator, show_progress = rlang::is_interactive(), show_problems = TRUE) {
  aggregate_files(files_data, aggregator, show_progress = show_progress, show_problems = show_progress)
}

# aggregate data from multiple files
# note that this function is a lot more universal than just for raw data
# it can be used for any isoverse aggregation and should be considered
# for porting to isoverse
aggregate_files <- function(files_data, aggregator, show_progress = rlang::is_interactive(), show_problems = TRUE, call = rlang::caller_call()) {

  # safety checks
  force(call)
  if(missing(files_data) || !is.data.frame(files_data)) cli_abort("{.var files_data} is not a data frame")
  files_req_cols <- c("file_path")
  if (length(missing <- setdiff(files_req_cols, names(files_data))))
    cli_abort("{.var files_data} is missing required column{?s} {.var {missing}}")
  if(missing(aggregator) || !is.data.frame(aggregator)) cli_abort("{.var aggregator} is not a data frame")
  aggregator_req_cols <- c("dataset", "column", "source", "default", "cast", "regexp", "func", "args")
  if (length(missing <- setdiff(aggregator_req_cols, names(aggregator))))
    cli_abort("{.var aggregator} is missing required column{?s} {.var {missing}}")
  if(nrow(files_data) == 0)
    cli_abort("there is nothing to aggregate, {.var files_data} has 0 rows")
  
  # get started
  start <- Sys.time()
  
  # progress bar
  pb <- NULL
  if (show_progress) { 
    old <- options(cli.progress_show_after = 0); on.exit(options(old))
    pb <- cli_progress_bar(
      total = nrow(files_data),
      format =
        paste(
          "Aggregating {pb_current}/{pb_total}",
          "file data {pb_bar} {pb_percent}",
          "| {pb_elapsed} | ETA{pb_eta} | {.emph {pb_status}}"
        )
    ); on.exit(cli_process_done(), add = TRUE)
    cli_progress_update(inc = 0, status = "initializing")
  }
  
  # aggregate files safely and with progress info
  aggregate_safely_with_progress <- function(file_path, i) {
    
    # progress
    if (show_progress) 
      cli_progress_update(id = pb, status = basename(file_path))
    
    # read file but catch errors in case there are any uncaught ones
    out <- 
      try_catch_cnds(
        aggregate_data(
          datasets = files_data[i, ] |> unlist(recursive = FALSE),
          aggregator = aggregator,
          show_problems = FALSE # show them later
        ),
        error_value = list(file_info = list(uid = NA), problems = tibble()),
      )
    
    # merge old, returned, and caught problems
    problems <- bind_rows(out$result$problems, out$conditions) |> mutate(new = TRUE)
    out$result$problems <- bind_rows(files_data$problems[[i]], problems) |>
      dplyr::mutate(uid = out$result$file_info$uid) |>
      dplyr::relocate("uid", .before = 1L)
    if (show_problems) {
      show_cnds(
        problems, call = call, 
        summary_message = sprintf("for {.emph %s}", basename(file_path))
      )
    }
    
    # return
    return(out$result)
  }
  
  # read files (separate calls to simplify backtraces)
  results <- files_data$file_path |> map2(1:nrow(files_data), aggregate_safely_with_progress)
  
  # combine results
  datasets <- c(unique(aggregator$dataset), "problems")
  results <- 
    as.list(datasets) |> setNames(datasets) |>
    purrr::map(~purrr::map(results, `[[`, .x) |> dplyr::bind_rows())
  
  # info
  if (show_progress) cli_progress_done(id = pb)
  details <- sprintf("%s (%d)", names(results), map_int(results, nrow))
  info <- format_inline(
    "{col_green(symbol$tick)} ",
    "Aggregated {.emph {head(details, -1)}} from {nrow(files_data)} file{?s} ", 
    "in {prettyunits::pretty_sec(as.numeric(Sys.time() - start, 'secs'))} ",
    summarize_cnds(
      results$problems |> dplyr::filter(!is.na(.data$new) & .data$new), 
      "but encountered {issues} {symbol$arrow_right} check with `orbi_get_problems(x)`")
  )
  cli_text(info)
  
  # return
  results$problems <- dplyr::select(results$problems, -"new")
  return(results)
}

# actual aggregation function
# note that this function is a lot more universal than just for raw data
# it can be used for any isoverse aggregation and should be considered
# for porting to isoverse
aggregate_data <- function(datasets, aggregator, show_problems = TRUE) {
  
  # check which datasets are available
  check_datasets <- function(aggregator) {
    if (length(missing <- setdiff(aggregator$dataset, names(datasets))) > 0)
      warning(format_inline("dataset{?s} {.var {missing}} do{?es/} not exist and will be skipped during aggregation"))
    aggregator |> filter(!dataset %in% missing)
  }
  
  # safety checks (run the check datasets with try_catch_cnds)
  aggregator <- aggregator |> check_datasets() |> try_catch_cnds(error_value = tibble())
  if (nrow(aggregator$result) == 0) cli_abort("there is nothing to aggregate")
  if (!"uid" %in% aggregator$result$column) 
    cli_abort("there is no {.var uid} (unique identifier) column in the aggregator")
  
  # find source columns and deal with any regexps that need to be resolved
  # shouldn't throw errors relevant for user so not run with try_catch_cnds
  find_source_columns <-  function(dataset, column, source, regexp) {
    # find source columns
    available_cols <- names(datasets[[dataset]])
    # find by regexp
    if (regexp && length(source) > 0) {
      sources <- grep(source[[1]], available_cols, value = TRUE, ignore.case = TRUE)
      if (length(sources) > 0) {
        # found some matching regexps
        columns <- gsub(source[[1]], column, sources, ignore.case = TRUE)
        sources <- as.list(sources)
      } else {
        # none found
        columns <- c()
        sources <- list(NULL)
      }
    } else if (length(source) > 0) {
      # direct matches
      columns <- column
      sources <- purrr::map(source, ~{ 
        matches <- intersect(.x, available_cols)
        if (length(matches) > 0) return(matches[1])
        return(NULL)
      })
      # no source columns found for same parameters -> can't use
      if (any(lengths(sources) == 0)) sources <- list(NULL)
      # recast for clearer use in functions
      else sources <- list(unlist(sources))
    } else {
      # no sources
      columns <- column
      sources <- list(NULL)
    }
    return(tibble(column = columns, source = sources))
  }
  
  # run the find source columns function
  aggregator_applied <- aggregator$result |>
    dplyr::mutate(
      cols = purrr::pmap(list(dataset, column, source, regexp), find_source_columns)
    ) |> 
    dplyr::select(-"column", -"source", -"regexp") |>
    tidyr::unnest("cols") |>
    filter(!is.na(.data$column))
  
  # aggregate value for dataset/column from source
  aggregate_value <- function(dataset, column, source, cast, func, args) {
    
    # generate expressions for sources
    sources <- map(source, ~expr((!!sym(dataset))[[(!!(.x))]]))
    
    if (!is.na(func)) {
      # with function
      args <- c(sources, args)
      expr <- rlang::expr((!!cast)((!!func)(!!!args)))
    } else {
      # without funtion
      expr <- rlang::expr((!!cast)(!!sources[[1]]))
    }
    
    # run with try catch so we can inject some more information
    value <- tryCatch(
      rlang::eval_tidy(expr, data = datasets),
      error = function(cnd) {
        cli_abort("encountered problems with {.emph {dataset}${column} = {as_label(expr)}}", parent = cnd)
      })
    return(value)
  }
  
  # wrap the value aggregation for a safe default and with try_catch_cnds
  aggregation_wrapper <- function(dataset, column, source, default, cast, func, args, ...) {
    default_expr <- rlang::expr((!!cast)(!!default))
    default_value <- rlang::eval_tidy(default_expr, data = datasets)
    # any source columns?
    if (length(source) == 0) return(list(result = default_value, conditions = NULL))
    # run safely
    out <-
      try_catch_cnds(
        aggregate_value(dataset, column, source, cast, func, args), 
        error_value = default_value
      )
    if (show_problems) show_cnds(out$conditions)
    return(out)
  }
  
  # run aggregation
  aggregator_applied$value <- 
    purrr::pmap(as.list(aggregator_applied), aggregation_wrapper)
  
  # separate call to simplify stack trace
  aggregator_applied <- aggregator_applied |> 
    tidyr::unnest_wider("value", simplify = FALSE)
  
  # get out uid
  uid <- filter(aggregator_applied, column == "uid")$result[[1]]
  if (is.null(uid)) cli_abort("{.var uid} is NULL")
  
  # make sure uid is in every dataset
  aggregator_w_uids <-
    dplyr::bind_rows(
      tibble(
        dataset = unique(aggregator_applied$dataset),
        column = "uid", result = list(uid)
      ),
      aggregator_applied |> filter(column != "uid")
    )
  
  # finalize datasets
  aggregator_summary <- 
    aggregator_w_uids |>
    filter(!purrr::map_lgl(result, is.null)) |>
    summarize(
      .by = "dataset",
      out = list(result |> setNames(column) |> as_tibble())
    )
  datasets <- aggregator_summary$out |> setNames(aggregator_summary$dataset)
  problems <- 
    dplyr::bind_rows(datasets$conditions, aggregator$conditions, 
                     aggregator_applied$conditions) |>
    dplyr::mutate(uid = !!uid, .before = 1L)
  
  # return with problems
  return(c(datasets, list(problems = problems))) 
}

