# isoorbi specific function ==========

#' Aggregate data from raw files
#'
#' This function allows dynamic aggregation and validation of data read by `orbi_read_raw`. Like [orbi_read_raw()], it is designed to be fail save by safely catching errors and reporting back on them (see [orbi_get_problems()]). This function should work out of the box for most files without additional modification of the `aggregator`.
#'
#' @param files_data the files read in by orbi_read_raw
#' @param aggregator an aggregator table ([orbi_start_aggregator()]) that defines which data should be aggregated and how. By default takes the predefined aggregator for raw data files stored in the [orbi_options()]
#' @inheritParams orbi_read_raw
#' @return a list of merged dataframes collected from the `files_data` based on the `aggregator` definitions
#' @export
orbi_aggregate_raw <- function(
  files_data,
  aggregator = orbi_get_option("raw_aggregator"),
  show_progress = rlang::is_interactive(),
  show_problems = TRUE
) {
  aggregate_files(
    files_data,
    aggregator,
    show_progress = show_progress,
    show_problems = show_progress
  )
}

#' Get data frame from aggregated data
#'
#' Retrieve a specific subset of the aggregated data into a single data frame by specifying which columns to take from each dataset (file_info, scans, peaks, etc.) using [dplyr::select()] syntax. If data from more than one dataset is selected (e.g. some columns from `scans` AND some from `peaks`), the datasets are combined with an [dplyr::inner_join()] using the columns listed in `by` (only the ones actually in the datasets). Joins that would lead to duplicated data entries (i.e. many-to-many joins) are not allowed and will throw an error to avoid unexpected replications of individual datapoints. If you really want to do such a join, you'll have to do it manually.
#' @param aggregated_data datasets aggregated from [orbi_aggregate_raw()]
#' @param file_info columns to get from the aggregated `file_info`, all [dplyr::select()] syntax is supported
#' @param scans columns to get from the aggregated `scans`, all [dplyr::select()] syntax is supported
#' @param peaks columns to get from the aggregated `peaks`, all [dplyr::select()] syntax is supported
#' @param spectra columns to get from the aggregated `spectra`, all [dplyr::select()] syntax is supported
#' @param problems columns to get from the aggregated `problems`, all [dplyr::select()] syntax is supported
#' @param by which columns to look for when joining datasets together. Make sure to include the relevant `by` columns in the selections of the individual datasets so they are joined correctly.
#' @return a tibble
#' @export
orbi_get_data <- function(
  aggregated_data,
  file_info = NULL,
  scans = NULL,
  peaks = NULL,
  spectra = NULL,
  problems = NULL,
  by = c("uid", "scan")
) {
  get_data(
    .ds = aggregated_data,
    file_info = {{ file_info }},
    scans = {{ scans }},
    peaks = {{ peaks }},
    spectra = {{ spectra }},
    problems = {{ problems }},
    by = by
  )
}

# design aggregators (general) =====

#' Dynamic data agreggator
#'
#' These functions allow definition of custom data aggregators for processing data extracted from raw files. An aggregator is run on each imported file and pulls together the relevant data users are interested in while making sure data formats are correct so that the aggregated data can be merged across several imported files for fast downstream processing.
#'
#' @param dataset name of the dataset in which to aggregate data, e.g. "file_info"
#' @param uid_source name of the column (inside the `dataset`) where the aggregators "uid" (unique ID) column should come from
#' @param cast what to cast the values of the resulting column to, most commonly `"as.character"`, `"as.integer"`, `"as.numeric"`, or `"as.factor"`. This is required to ensure all aggregated values have the correct data type.
#' @param regexp whether source columm names should be interpreted as a regular expressions for the purpose of finding the relevant column(s). Note if `regexp = TRUE`, the search for the source column always becomes case-insensitive so this can also be used for a direct match of a source column whose upper/lower casing can be unreliable.
#' @param func name of a processing function to apply before casting the value with the `cast` function. This is optional and can be used to conduct more elaborate preprocessing of a data or combining data from multiple source columns in the correct way (e.g. pasting together from multiple columns).
#' @param args an optional list of arguments to pass to the `func` in addition to the values coming from the source colummn(s)
#' @return a tibble holding all the columns that the aggregator will generate when run against data from a file
#' @describeIn orbi_aggregator start the aggregator, requires definition of where the unique ID of a dataset comes from
#' @export
orbi_start_aggregator <- function(
  dataset,
  uid_source,
  cast = "as.factor",
  regexp = FALSE,
  func = NULL,
  args = NULL
) {
  orbi_add_aggregator(
    tibble(),
    dataset,
    column = "uid",
    source = uid_source,
    cast = cast,
    regexp = regexp,
    func = func,
    args = args
  )
}

#' add aggregator
#' note on regexp --> always case insensitive!
#' note overwrites an existing aggregator entry for the same dataset and column
#' @param aggregator the aggregator table generated by [orbi_start_aggregator()] or passed from a previous call to [orbi_add_aggregator()] for constructing the entire aggregator by piping
#' @param column the name of the column in which data should be stored
#' @param default the default value if no `source` columns can be found or another error is encountered during aggregatio. Note that the `default` value will also be processed with the function in `cast` to make sure it has the correct data type.
#' @param source single character column name or vector of column names (if alternatives could be the source) where in the `dataset` to find data for the `column`. If a vector of multiple column names is provided (e.g. `source = c("a1", "a2")`), the first column name that's found during processing of a dataset will be used and passed to the function defined in `func` (if any) and then the one defined in `cast`. To provide multiple parameters from the data to `func`, define a list instead of a vector `source = list("a", "b", "c")` or if multiple alternative columns can be the source for any of the arguments, define as `source = list(c("a1", "a2"), "b", c("c1", "c2", "c3"))`
#' @describeIn orbi_aggregator add additional column to aggregate data for
#' @export
orbi_add_aggregator <- function(
  aggregator,
  dataset,
  column,
  source = column,
  default = NA,
  cast = "as.character",
  regexp = FALSE,
  func = NULL,
  args = NULL
) {
  # checks
  stopifnot(
    "`aggregator` must be a data frame" = !missing(aggregator) &&
      is_tibble(aggregator),
    "`dataset` must be a scalar character" = !missing(dataset) &&
      is_scalar_character(dataset),
    "`column` must be a scalar character" = !missing(column) &&
      is_scalar_character(column),
    "`source` must a character vector of list" = is_character(source) ||
      is_list(source),
    "`regexp` must be TRUE or FALSE" = is_scalar_logical(regexp),
    "`cast` must be a scalar character" = is_scalar_character(cast),
    "if provided, `func` must be a scalar character" = is.null(func) ||
      is_scalar_character(func),
    "if provided, `args` must be a list" = is.null(args) || is_list(args)
  )
  if (!is.list(source)) {
    source <- list(source)
  }

  # function checks - pkg::func not supported yet (e.g. forcats::as_factor)! need to load the namespace, solution needed?
  if (!exists(cast, mode = "function")) {
    cli_abort("function {.fn {cast}} could not be found")
  }
  if (!is.null(func) && !exists(func, mode = "function")) {
    cli_abort("function {.fn {func}} could not be found")
  }

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

# if we make aggregator a class this could just be the show method
summarize_aggregator <- function(aggregator) {
  aggregator |>
    dplyr::mutate(
      source = purrr::map_chr(
        source,
        ~ {
          sprintf(
            "arg %d: '%s'",
            seq_along(.x),
            purrr::map_chr(.x, paste, collapse = "' or '")
          ) |>
            paste(collapse = "; ")
        }
      ),
      args = purrr::map_chr(
        args,
        ~ {
          if (length(.x) > 0) {
            format_inline("args: {.x}")
          } else {
            NA_character_
          }
        }
      )
    )
}

# aggregate raw file data (general) ========

# aggregate data from multiple files
# note that this function is a lot more universal than just for raw data
# it can be used for any isoverse aggregation and should be considered
# for porting to isoverse
# note: should this add a class type to the returned list structure to print helpful summary statistics?
aggregate_files <- function(
  files_data,
  aggregator,
  show_progress = rlang::is_interactive(),
  show_problems = TRUE,
  call = rlang::caller_call()
) {
  # safety checks
  force(call)
  if (missing(files_data) || !is.data.frame(files_data)) {
    cli_abort("{.var files_data} is not a data frame")
  }
  files_req_cols <- c("file_path")
  if (length(missing <- setdiff(files_req_cols, names(files_data)))) {
    cli_abort(
      "{.var files_data} is missing required column{?s} {.var {missing}}"
    )
  }
  if (missing(aggregator) || !is.data.frame(aggregator)) {
    cli_abort("{.var aggregator} is not a data frame")
  }
  aggregator_req_cols <- c(
    "dataset",
    "column",
    "source",
    "default",
    "cast",
    "regexp",
    "func",
    "args"
  )
  if (length(missing <- setdiff(aggregator_req_cols, names(aggregator)))) {
    cli_abort(
      "{.var aggregator} is missing required column{?s} {.var {missing}}"
    )
  }
  if (nrow(files_data) == 0) {
    cli_abort("there is nothing to aggregate, {.var files_data} has 0 rows")
  }

  # get started
  start <- Sys.time()

  # progress bar
  pb <- NULL
  if (show_progress) {
    old <- options(cli.progress_show_after = 0)
    on.exit(options(old))
    pb <- cli_progress_bar(
      total = nrow(files_data),
      format = paste(
        "Aggregating {pb_current}/{pb_total}",
        "file data {pb_bar} {pb_percent}",
        "| {pb_elapsed} | ETA{pb_eta} | {.emph {pb_status}}"
      )
    )
    on.exit(cli_process_done(), add = TRUE)
    cli_progress_update(id = pb, inc = 0, status = "initializing")
  }

  # aggregate files safely and with progress info
  aggregate_safely_with_progress <- function(file_path, i) {
    # progress
    if (show_progress) {
      cli_progress_update(id = pb, status = basename(file_path))
    }

    # read file but catch errors in case there are any uncaught ones
    out <-
      try_catch_cnds(
        aggregate_data(
          datasets = files_data[i, ] |> unlist(recursive = FALSE),
          aggregator = aggregator,
          show_problems = FALSE # show them later
        ),
        error_value = list(file_info = list(uid = NA), problems = tibble()),
        catch_errors = !orbi_get_option("debug")
      )

    # filepath update
    if ("file_info" %in% names(out$result)) {
      out$result$file_info <- out$result$file_info |>
        dplyr::as_tibble() |>
        dplyr::mutate(file_path = !!file_path, .after = 1L)
    }

    # merge old, returned, and caught problems
    problems <- bind_rows(out$result$problems, out$conditions) |>
      mutate(new = TRUE)
    out$result$problems <- bind_rows(files_data$problems[[i]], problems) |>
      dplyr::mutate(uid = out$result$file_info$uid) |>
      dplyr::relocate("uid", .before = 1L)
    if (show_problems) {
      show_cnds(
        problems,
        call = call,
        summary_message = sprintf("for {.emph %s}", basename(file_path))
      )
    }

    # return
    return(out$result)
  }

  # read files (separate calls to simplify backtraces)
  results <- files_data$file_path |>
    map2(1:nrow(files_data), aggregate_safely_with_progress)

  # combine results
  datasets <- c(unique(aggregator$dataset), "problems")
  results <-
    as.list(datasets) |>
    setNames(datasets) |>
    purrr::map(~ purrr::map(results, `[[`, .x) |> dplyr::bind_rows())

  # info
  if (show_progress) {
    cli_progress_done(id = pb)
  }
  n_rows <- map_int(results, nrow) |>
    prettyunits::pretty_num() |>
    # take care of leading/trailing whitespaces
    gsub(pattern = "(^ +| +$)", replacement = "")
  details <- sprintf("{col_blue('%s')} (%s)", names(results), n_rows)
  info <- format_inline(
    "{col_green(symbol$tick)} ",
    "Aggregated {head(details, -1)} from {nrow(files_data)} file{?s} ",
    "in {prettyunits::pretty_sec(as.numeric(Sys.time() - start, 'secs'))} ",
    summarize_cnds(
      results$problems |> dplyr::filter(!is.na(.data$new) & .data$new),
      "but encountered {issues} {symbol$arrow_right} check with `orbi_get_problems(x)`"
    )
  )
  cli_text(info)

  # clean up problems
  results$problems <- dplyr::select(results$problems, -"new")

  # details on columns
  cli_bullets(
    purrr::map2_chr(
      results,
      names(results),
      ~ format_inline(
        "{symbol$arrow_right} {col_blue(.y)}: ",
        "{.emph {names(.x)}}"
      )
    ) |>
      setNames(rep(" ", length(results)))
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
    if (length(missing <- setdiff(aggregator$dataset, names(datasets))) > 0) {
      warning(format_inline(
        "dataset{?s} {.var {missing}} do{?es/} not exist and will be skipped during aggregation"
      ))
    }
    aggregator |> filter(!.data$dataset %in% missing)
  }

  # safety checks (run the check datasets with try_catch_cnds)
  aggregator <- aggregator |>
    check_datasets() |>
    try_catch_cnds(
      error_value = tibble(),
      catch_errors = !orbi_get_option("debug")
    )
  if (nrow(aggregator$result) == 0) {
    cli_abort("there is nothing to aggregate")
  }
  if (!"uid" %in% aggregator$result$column) {
    cli_abort(
      "there is no {.var uid} (unique identifier) column in the aggregator"
    )
  }

  # find source columns and deal with any regexps that need to be resolved
  # shouldn't throw errors relevant for user so not run with try_catch_cnds
  find_source_columns <- function(dataset, column, source, regexp) {
    # find source columns
    available_cols <- names(datasets[[dataset]])
    # find by regexp
    if (regexp && length(source) > 0) {
      sources <- grep(
        source[[1]],
        available_cols,
        value = TRUE,
        ignore.case = TRUE
      )
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
      sources <- purrr::map(
        source,
        ~ {
          matches <- intersect(.x, available_cols)
          if (length(matches) > 0) {
            return(matches[1])
          }
          return(NULL)
        }
      )
      # no source columns found for same parameters -> can't use
      if (any(lengths(sources) == 0)) {
        sources <- list(NULL)
      } else {
        # recast for clearer use in functions
        sources <- list(unlist(sources))
      }
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
      cols = purrr::pmap(
        list(.data$dataset, .data$column, .data$source, .data$regexp),
        find_source_columns
      )
    ) |>
    dplyr::select(-"column", -"source", -"regexp") |>
    tidyr::unnest("cols") |>
    filter(!is.na(.data$column))

  # aggregate value for dataset/column from source
  aggregate_value <- function(dataset, column, source, cast, func, args) {
    # generate expressions for sources
    sources <- map(source, ~ expr((!!sym(dataset))[[(!!(.x))]]))

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
        cli_abort(
          "encountered problems with {.emph {dataset}${column} = {as_label(expr)}}",
          parent = cnd
        )
      }
    )
    return(value)
  }

  # wrap the value aggregation for a safe default and with try_catch_cnds
  aggregation_wrapper <- function(
    dataset,
    column,
    source,
    default,
    cast,
    func,
    args,
    ...
  ) {
    default_expr <- rlang::expr((!!cast)(!!default))
    default_value <- rlang::eval_tidy(default_expr, data = datasets)
    # any source columns?
    if (length(source) == 0) {
      return(list(result = default_value, conditions = NULL))
    }
    # run safely
    out <-
      try_catch_cnds(
        aggregate_value(dataset, column, source, cast, func, args),
        error_value = default_value,
        catch_errors = !orbi_get_option("debug")
      )
    if (show_problems) {
      show_cnds(out$conditions)
    }
    return(out)
  }

  # run aggregation
  aggregator_applied$value <-
    purrr::pmap(as.list(aggregator_applied), aggregation_wrapper)

  # separate call to simplify stack trace
  aggregator_applied <- aggregator_applied |>
    tidyr::unnest_wider("value", simplify = FALSE)

  # get out uid
  uid <- filter(aggregator_applied, .data$column == "uid")$result[[1]]
  if (is.null(uid)) {
    cli_abort("{.var uid} is NULL")
  }

  # make sure uid is in every dataset
  aggregator_w_uids <-
    dplyr::bind_rows(
      tibble(
        dataset = unique(aggregator_applied$dataset),
        column = "uid",
        result = list(uid)
      ),
      aggregator_applied |> filter(.data$column != "uid")
    )

  # finalize datasets
  aggregator_summary <-
    aggregator_w_uids |>
    filter(!purrr::map_lgl(.data$result, is.null)) |>
    summarize(
      .by = "dataset",
      out = list(.data$result |> setNames(.data$column) |> as_tibble())
    )
  datasets <- aggregator_summary$out |> setNames(aggregator_summary$dataset)
  problems <-
    dplyr::bind_rows(
      datasets$conditions,
      aggregator$conditions,
      aggregator_applied$conditions
    ) |>
    dplyr::mutate(uid = !!uid, .before = 1L)

  # return with problems
  return(c(datasets, list(problems = problems)))
}

# get data from aggregated ===========

# get data from an aggregated list of datasets via inner joins
# @param .ds the list of datasets (uses .ds to avoid accidental partial mapping by ...)
# @param ... named arguments for each dataset that should be included with a tidyselect expression for the columns to include
# @param by which columns to use for join by operations (if there are any)
# @param relationship passed to inner_join
get_data <- function(
  .ds,
  ...,
  by = c(),
  call = rlang::caller_env(),
  relationship = NULL
) {
  # basic safety checks
  stopifnot(
    "`.ds` must be a list" = !missing(.ds) && is_list(.ds),
    "`.ds` must contain at least one data frame" = length(.ds) > 0 &&
      sum(sapply(.ds, is.data.frame) > 0),
    "`by` must be a character vector" = is_empty(by) || is_character(by)
  )

  # any selectors?
  selectors <- rlang::enquos(...)
  selectors <- selectors[!purrr::map_lgl(selectors, quo_is_null)]
  if (length(selectors) == 0) {
    cli_abort(c(
      "no dataset column selections provided",
      "i" = "available dataset{?s}: {.emph {names(.ds)}}"
    ))
  }

  # valid selectors?
  if (any(missing <- !names(selectors) %in% names(.ds))) {
    cli_abort(
      c(
        "dataset{?s} not in the data: {.emph {names(selectors)[missing]}}",
        "i" = "available dataset{?s}: {.emph {names(.ds)}}"
      ),
      call = call
    )
  }

  # quick loop (better errors than map for the small number of .ds)
  join_bys <- list()
  out <- tibble::tibble()
  for (i in seq_along(selectors)) {
    # fetch
    new_data <- tryCatch(
      .ds[[names(selectors)[i]]] |> dplyr::select(!!selectors[[i]]),
      error = function(cnd) {
        cli_abort(
          c(
            "encountered an error selecting columns for {.emph {names(selectors)[i]}}",
            "i" = "columns in {.emph {names(selectors)[i]}}: {.var {names(.ds[[names(selectors)[i]]])}}"
          ),
          parent = cnd,
          call = call
        )
      }
    )
    # join
    if (i > 1) {
      join_by <- intersect(by, names(out)) |> intersect(names(new_data))
      if (length(join_by) == 0) {
        cli_abort(
          c(
            paste(
              "unclear how to join {.emph {names(selectors)[i]}}",
              "with {.emph {names(selectors)[1:(i-1)]}}"
            ),
            "i" = if (length(by) == 0) {
              "there are no join columns defined in {.var by}"
            } else {
              "allowed join columns: {.var {by}}"
            },
            "i" = "columns in {.emph {names(selectors)[i]}}: {.var {names(new_data)}}",
            "i" = "columns in {.emph {names(selectors)[1:(i-1)]}}: {.var {names(out)}}"
          ),
          call = call
        )
      }
      out <-
        tryCatch(
          dplyr::inner_join(
            out,
            new_data,
            by = join_by,
            relationship = relationship
          ),
          warning = function(cnd) {
            cli_abort(
              c(
                paste(
                  "encountered warning when joining {.emph {names(selectors)[i]}}",
                  "with {.emph {names(selectors)[1:(i-1)]}} by {.var {join_by}}"
                ),
                "i" = "are you sure the by column{?s} ({.var {join_by}}) {?is/are} sufficient and this operation is really what is intended?"
              ),
              parent = cnd,
              call = call
            )
          }
        )
      join_bys <- c(join_bys, join_by)
    } else {
      # just one dataset
      out <- new_data
    }
  }

  # info
  n_rows <-
    purrr::map_int(.ds[names(selectors)], nrow) |>
    prettyunits::pretty_num() |>
    # take care of leading/trailing whitespaces
    gsub(pattern = "(^ +| +$)", replacement = "")

  details <-
    if (length(selectors) == 1) {
      sprintf("{col_blue('%s')}", names(selectors))
    } else {
      sprintf("{col_blue('%s')} (%s)", names(selectors), n_rows)
    }

  format_inline(
    "Retrieved {prettyunits::pretty_num(nrow(out))} records from {qty(length(selectors))}",
    "{?/the combination of }{details}{?/ via }{?/{.var {unique(unlist(join_bys))}}}"
  ) |>
    cli_alert_success(wrap = TRUE)

  # return
  return(out)
}
