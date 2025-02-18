# isoorbi specific function ==========

#' aggregate data from raw files
#' @param files_data the files read in by orbi_read_raw
#' @export
orbi_aggregate_raw <- function(files_data, aggregator, show_progress = rlang::is_interactive(), show_problems = TRUE) {
  aggregate_files(files_data, aggregator, show_progress = show_progress, show_problems = show_progress)
}

# design aggregators (general) =====

#' start the aggregator dataset
#' @export
start_aggregator <- function(dataset, uid_source, cast, regexp = FALSE, func = NULL, args = NULL) {
  add_aggregator(tibble(), dataset, column = "uid", source = uid_source, cast = cast, regexp = regexp, func = func, args = args)
}

#' add aggregator
#' note on regexp --> always case insensitive!
#' note overwrites an existing aggregator entry for the same dataset and column
#' @param source single character column name or vector of column names if alternatives could be the source
#' @param args additional arguments to pass to func
#' @export
add_aggregator <- function(aggregator, dataset, column, source = column, default = NA, cast = "as.character", regexp = FALSE, func = NULL, args = NULL) {
  
  # checks
  if(!is_tibble(aggregator)) cli_abort("{.var aggregator} is not a data frame")
  if(!is_scalar_character(dataset)) cli_abort("{.var dataset} is not a scalar character")
  if(!is_scalar_character(column)) cli_abort("{.var column} is not a scalar character")
  if(!is_character(source) && !is_list(source)) cli_abort("{.var source} is not a character vector or list")
  if (!is.list(source)) source <- list(source)
  if(!is_scalar_logical(regexp)) cli_abort("{.var regexp} is not a TRUEor FALSE")
  if(!is_scalar_character(cast) || !exists(cast, mode = "function")) 
    cli_abort("{.var cast} is not the name of a function")
  if(!is.null(func) && (!is_scalar_character(func) || !exists(func, mode = "function"))) 
    cli_abort("{.var func} is not the name of a function")
  if(!is.null(args) && !is_list(args)) cli_abort("{.var agrs} must be a list if provided")
  
  # check if default evaluates without error to avoid unexpected/uncaught errors later on
  do.call(cast, args = list(default))
  
  aggregator |>
    # overwrite existing if there are any
    filter(!(dataset == !!dataset & column == !!column)) |>
    # add new
    dplyr::bind_rows(
      tibble(
        dataset = dataset,
        column = column,
        source = list(source),
        default = default,
        regexp = regexp,
        cast = cast,
        func = if (!is.null(func)) func else NA_character_,
        args = if (!is.null(args)) list(args) else list(NULL)
      )
    )
}

# aggregate raw file data (general) ========

# aggregate data from multiple files
# note that this function is a lot more universal than just for raw data
# it can be used for any isoverse aggregation and should be considered
# for porting to isoverse
# note: should this add a class type to the returned list structure to print helpful summary statistics?
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
    
    # filepath update
    out$result$file_info <- out$result$file_info |>
      dplyr::mutate(file_path = !!file_path, .after = 1L)
    
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
  n_rows <- map_int(results, nrow) |> 
    prettyunits::pretty_num("nopad") |> stringr::str_remove(" ?$")
  details <- sprintf("{col_blue('%s')} (%s)", names(results), n_rows)
  info <- format_inline(
    "{col_green(symbol$tick)} ",
    "Aggregated {head(details, -1)} from {nrow(files_data)} file{?s} ", 
    "in {prettyunits::pretty_sec(as.numeric(Sys.time() - start, 'secs'))} ",
    summarize_cnds(
      results$problems |> dplyr::filter(!is.na(.data$new) & .data$new), 
      "but encountered {issues} {symbol$arrow_right} check with `orbi_get_problems(x)`")
  )
  cli_text(info)
  
  # clean up problems
  results$problems <- dplyr::select(results$problems, -"new")
  
  # details on columns
  cli_bullets(
    purrr::map2_chr(
      results, names(results), ~
        format_inline(
          "{symbol$arrow_right} {col_blue(.y)}: ",
          "{.emph {names(.x)}}")) |> 
      setNames(rep(" ", length(results))
      )
  )
  
  # return
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

