# isoorbi specific function ==========

#' Aggregate data from raw files
#'
#' This function allows dynamic aggregation and validation of data read by [orbi_read_raw()]. Like [orbi_read_raw()], it is designed to be fail save by safely catching errors and reporting back on them (see [orbi_get_problems()]). This function should work out of the box for most files without additional modification of the `aggregator`.
#'
#' @param files_data the files read in by [orbi_read_raw()]
#' @param aggregator typically the name of a registered aggregator (see all with `orbi_get_option("aggregators")`),
#' default is the "standard" aggregator included in the package (`orbi_get_aggregator("standard")`).
#' Other options are "minimal" (`orbi_get_aggregator("minimal")`) and "extended" (`orbi_get_aggregator("extended")`).
#' The `aggregator` parameter can can also directly be an aggregator tibble (created/modified with [orbi_start_aggregator()]
#' and/or [orbi_add_to_aggregator()]) that defines which data should be aggregated and how.
#' @inheritParams orbi_read_raw
#' @return a list of merged dataframes collected from the `files_data` based on the `aggregator` definitions
#' @export
orbi_aggregate_raw <- function(
  files_data,
  aggregator = "standard",
  show_progress = rlang::is_interactive(),
  show_problems = TRUE
) {
  check_arg(
    aggregator,
    is_scalar_character(aggregator) || is(aggregator, "orbi_aggregator")
  )
  if (is.character(aggregator)) {
    aggregator <- orbi_get_aggregator(aggregator)
  }
  agg_data <-
    aggregate_files(
      files_data,
      aggregator,
      show_progress = show_progress,
      show_problems = show_problems
    )
  class(agg_data) <- unique(c("orbi_aggregated_data"), class(agg_data))
  return(agg_data)
}

#' @export
print.orbi_aggregated_data <- function(x, ...) {
  cli_rule(
    center = "{.strong aggregated data from {nrow(x$file_info)} raw file{?s} - retrieve with orbi_get_data()}"
  )

  # details on columns
  x |>
    purrr::map2_chr(
      names(x),
      function(dataset, name) {
        # summarize problems
        if (name == "problems") {
          return(
            summarize_cnds(
              dataset,
              include_symbol = FALSE,
              include_call = FALSE,
              summary_format = paste0(
                "{symbol$arrow_right} {cli::col_blue('",
                name,
                "')}: ",
                if (nrow(dataset) > 0) {
                  "has a total of {issues} "
                } else {
                  "has {issues}"
                },
                if (nrow(dataset) > 0) {
                  "{symbol$arrow_right} check with {.strong orbi_get_problems(x)}"
                }
              )
            )
          )
        }

        # all other fields
        n_rows <- nrow(dataset) |> format_number()
        n_na <- purrr::map_int(dataset, ~ sum(is.na(.x)))
        cols <- names(dataset) |>
          sprintf(fmt = "{.field %s}") |>
          paste0(if_else(
            n_na > 0,
            sprintf(" ({col_yellow('%s NA')})", n_na |> format_number()),
            ""
          ))
        unused_cols <- sprintf(
          "{.emph {col_yellow('%s')}}",
          attr(dataset, "unused_columns")
        )
        format_inline(
          "{symbol$arrow_right} {cli::col_blue(name)} ({n_rows}): ",
          "{glue::glue_collapse(cols, sep = ', ')}",
          if (length(unused_cols) > 0) {
            "; ({.emph not aggregated}: {glue::glue_collapse(unused_cols, sep = ', ')})"
          }
        )
      }
    ) |>
    cli_bullets() # no |> cli() at the to keep paragraphs a bit separated in knitted doc
}

#' @export
knit_print.orbi_aggregated_data <- function(x, ...) {
  print(x, ...)
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
  file_info = c("filename"),
  scans = NULL,
  peaks = NULL,
  spectra = NULL,
  problems = NULL,
  by = c("uidx", "scan.no")
) {
  spectra_quo <- enquo(spectra)
  # get data
  out <- get_data(
    .ds = aggregated_data,
    file_info = {{ file_info }},
    scans = {{ scans }},
    peaks = {{ peaks }},
    spectra = !!spectra_quo,
    problems = {{ problems }},
    by = by
  )
  # warn about missing spectra
  if (
    !quo_is_null(spectra_quo) &&
      "spectra" %in% names(aggregated_data) &&
      nrow(aggregated_data$spectra) == 0
  ) {
    cli_text(
      "{cli::col_yellow('!')} {.strong Warning}: there are no {.field spectra} in the data, make sure to include them when reading the raw files e.g. with {.strong orbi_read_raw(include_spectra = c(1, 10, 100))}"
    )
  }
  return(out)
}

# design aggregators (general) ========

# helper function to turn something into an aggregator tibble
as_aggregator <- function(x, name = attr(x, "name")) {
  attr(x, "name") <- name
  class(x) <- unique(c("orbi_aggregator", class(x)))
  return(x)
}

#' Dynamic data agreggator
#'
#' These functions allow definition of custom data aggregators for processing data extracted from raw files. An aggregator is run on each imported file and pulls together the relevant data users are interested in while making sure data formats are correct so that the aggregated data can be merged across several imported files for fast downstream processing.
#'
#' @param name a descriptive name for the aggregator. This name is automatically used as the default name when registering the aggregator via [orbi_register_aggregator()].
#' @return an orbi aggregator tibble
#' @describeIn orbi_aggregator starts the aggregator
#' @export
orbi_start_aggregator <- function(name) {
  check_arg(
    name,
    !missing(name) && is_scalar_character(name),
    "must be a string"
  )
  tibble(
    dataset = character(),
    column = character(),
    source = list(),
    default = list(),
    regexp = logical(),
    cast = character(),
    func = character(),
    args = list()
  ) |>
    as_aggregator(name = name)
}

# add to aggregator
#' @param aggregator the aggregator table generated by [orbi_start_aggregator()] or passed from a previous call to [orbi_add_to_aggregator()] for constructing the entire aggregator by piping
#' @param dataset the name of the dataset to aggregate from (`file_info`, `scans`, `peaks`, `spectra`)
#' @param column the name of the column in which data should be stored
#' @param source single character column name or vector of column names (if alternatives could be the source) where in the `dataset` to find data for the `column`. If a vector of multiple column names is provided (e.g. `source = c("a1", "a2")`), the first column name that's found during processing of a dataset will be used and passed to the function defined in `func` (if any) and then the one defined in `cast`. To provide multiple parameters from the data to `func`, define a list instead of a vector `source = list("a", "b", "c")` or if multiple alternative columns can be the source for any of the arguments, define as `source = list(c("a1", "a2"), "b", c("c1", "c2", "c3"))`
#' @param default the default value if no `source` columns can be found or another error is encountered during aggregatio. Note that the `default` value will also be processed with the function in `cast` to make sure it has the correct data type.
#' @param cast what to cast the values of the resulting column to, most commonly `"as.character"`, `"as.integer"`, `"as.numeric"`, or `"as.factor"`. This is required to ensure all aggregated values have the correct data type.
#' @param regexp whether source columm names should be interpreted as a regular expressions for the purpose of finding the relevant column(s). Note if `regexp = TRUE`, the search for the source column always becomes case-insensitive so this can also be used for a direct match of a source column whose upper/lower casing can be unreliable. If a column is matched by a regexp and also by a direct aggregator rule, the direct aggregator rule takes precedence.
#' @param func name of a processing function to apply before casting the value with the `cast` function. This is optional and can be used to conduct more elaborate preprocessing of a data or combining data from multiple source columns in the correct way (e.g. pasting together from multiple columns).
#' @param args an optional list of arguments to pass to the `func` in addition to the values coming from the source colummn(s)
#' @describeIn orbi_aggregator add additional column to aggregate data for. Overwrites an existing aggregator entry for the same dataset and column if it already exists.
#' @export
orbi_add_to_aggregator <- function(
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
  # safety checks
  check_arg(
    aggregator,
    !missing(aggregator) &&
      is_tibble(aggregator) &&
      is(aggregator, "orbi_aggregator"),
    format_inline("must be an {cli::col_magenta('orbi_aggregator')} tibble")
  )
  check_arg(
    dataset,
    !missing(dataset) && is_scalar_character(dataset),
    "must be a string"
  )
  check_arg(
    column,
    !missing(column) && is_scalar_character(column),
    "must be a string"
  )
  check_arg(
    source,
    is_character(source) || is_list(source),
    "must be a character vector or list"
  )
  check_arg(
    regexp,
    is_scalar_logical(regexp),
    "must be TRUE or FALSE"
  )
  check_arg(cast, is_scalar_character(cast), "must be a string")
  check_arg(
    func,
    is.null(func) || is_scalar_character(func),
    "- if provided - must be a string"
  )
  check_arg(
    args,
    is.null(args) || is_list(args),
    "- if provided - must be a list"
  )

  # make surce source is a list
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
        default = list(default),
        regexp = regexp,
        cast = cast,
        func = if (!is.null(func)) func else NA_character_,
        args = if (!is.null(args)) list(args) else list(NULL)
      )
    ) |>
    # make sure it's an aggregator
    as_aggregator()
}

# register an aggregator
#' @describeIn orbi_aggregator register an aggregator in the isoorbi options so it can be retrieved with [orbi_get_aggregator()]
#' @export
orbi_register_aggregator <- function(
  aggregator,
  name = attr(aggregator, "name")
) {
  # checks
  check_arg(
    aggregator,
    !missing(aggregator) &&
      is_tibble(aggregator) &&
      is(aggregator, "orbi_aggregator"),
    format_inline("must be an {cli::col_magenta('orbi_aggregator')} tibble")
  )
  check_arg(name, is_scalar_character(name), "must be a string")
  aggs <- orbi_get_option("aggregators")
  aggs[[name]] <- as_aggregator(aggregator, name = name)
  orbi_options(list(aggregators = aggs))
  # return invisibly for piping
  return(invisible(aggregator))
}

# retrieve a registered aggregator
#' @describeIn orbi_aggregator retrieve a registered aggregator (get all aggregators with `orbi_get_option("aggregators")`)
#' @export
orbi_get_aggregator <- function(name) {
  # safety checks
  check_arg(
    name,
    !missing(name) && is_scalar_character(name),
    "must be a string"
  )
  aggs <- orbi_get_option("aggregators")
  if (!name %in% names(aggs)) {
    cli_abort(c(
      "{.emph {.strong {name}}} is not a registered aggregator",
      "i" = "available aggregator{?s}: {.emph {.strong {names(aggs)}}}"
    ))
  }
  return(aggs[[name]])
}

#' @export
print.orbi_aggregator <- function(x, ...) {
  cli_rule(center = "{.strong Aggregator {.emph {attr(x, 'name')}}}")
  if (nrow(x) > 0) {
    summarize_aggregator(x) |>
      dplyr::mutate(
        label = purrr::pmap_chr(
          list(
            .data$column,
            .data$value,
            .data$default,
            .data$regexp,
            .data$source
          ),
          function(col, value, default, regex, source) {
            if (regex) {
              matches <- regmatches(
                source,
                gregexpr("\\([^?)][^()]*\\)", source)
              )[[
                1
              ]]
              for (i in seq_along(matches)) {
                col <- gsub(sprintf("\\%d", i), matches[[i]], col, fixed = TRUE)
              }
              col <- gsub("\\", "\\\\", col, fixed = TRUE)
              format_inline(
                "{symbol$arrow_right} {cli::col_magenta(col)} = {.emph {value}}"
              )
            } else if (!is.na(default[[1]])) {
              format_inline(
                "{symbol$arrow_right} {.field {col}} = {.emph {value}} - {col_yellow('if source is missing')}: {.field {col}} = {.emph {default[[1]]}}"
              )
            } else {
              format_inline(
                "{symbol$arrow_right} {.field {col}} = {.emph {value}}"
              )
            }
          }
        )
      ) |>
      dplyr::summarise(
        .by = "dataset",
        label = list(c(
          format_inline(
            "{.strong Dataset} {cli::col_blue(.data$dataset[[1]])}:"
          ),
          paste0("\u00a0", .data$label)
        ))
      ) |>
      dplyr::pull(.data$label) |>
      unlist() |>
      cli_bullets_raw() |>
      cli()
  }
}

#' @export
knit_print.orbi_aggregator <- function(x, ...) {
  print(x, ...)
}

# summarize the aggregator this way
summarize_aggregator <- function(aggregator) {
  if (nrow(aggregator) == 0) {
    return(
      aggregator |>
        dplyr::mutate(
          value = character(0),
          default = character(0)
        )
    )
  }

  # generate text descriptor for the value
  generate_value_descriptor <- function(source, cast, regexp, func, args) {
    sources <- map(
      source,
      ~ {
        if (regexp) {
          # pseudo code (all_matches doesn't exist)
          expr(all_matches(!!.x[[1]]))
        } else if (length(.x) > 1) {
          # pseudo code (one_of doesn't exist)
          expr(one_of(!!!syms(.x)))
        } else {
          expr(!!sym(.x))
        }
      }
    )

    if (!is.na(func)) {
      # with function
      args <- c(sources, args)
      expr <- rlang::expr((!!cast)((!!func)(!!!args)))
    } else {
      # without funtion
      expr <- rlang::expr((!!cast)(!!sources[[1]]))
    }
    return(expr_text(expr))
  }

  # generate text descriptor for default
  generate_default_descriptor <- function(default, cast) {
    if (is.na(default[[1]])) {
      return(NA_character_)
    }
    expr <- rlang::expr((!!cast)(!!default[[1]]))
    return(expr_text(expr))
  }

  # summarize
  aggregator |>
    dplyr::mutate(
      value = purrr::pmap_chr(
        list(
          .data$source,
          .data$cast,
          .data$regexp,
          .data$func,
          .data$args
        ),
        generate_value_descriptor
      ),
      default = purrr::map2_chr(
        .data$default,
        .data$cast,
        generate_default_descriptor
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
  # keep track of current env to anchor progress bars
  root_env <- current_env()

  # safety checks
  files_req_cols <- c("filepath")
  check_tibble(files_data, files_req_cols)
  check_arg(
    aggregator,
    !missing(aggregator) &&
      is_tibble(aggregator) &&
      is(aggregator, "orbi_aggregator"),
    format_inline("must be an {cli::col_magenta('orbi_aggregator')} tibble")
  )
  if (nrow(files_data) == 0) {
    cli_abort("there is nothing to aggregate, {.var files_data} has 0 rows")
  }

  # get started
  start <- Sys.time()

  # info
  start <- start_info(
    "is aggregating data from raw file {pb_current}/{pb_total} using {.emph {.strong {attr(aggregator, 'name')}}} aggregator {pb_bar} ",
    "file data {pb_bar} {pb_percent}",
    "| {pb_elapsed} | ETA{pb_eta} | {.file {pb_status}}",
    pb_total = nrow(files_data),
    pb_status = "",
    show_progress = show_progress,
    .env = root_env
  )

  # aggregate files safely and with progress info
  aggregate_safely_with_progress <- function(filepath, uidx) {
    # progress
    if (!is.null(start$pb)) {
      cli_progress_update(
        id = start$pb,
        status = basename(filepath),
        .envir = root_env
      )
    }

    # start timer
    file_start <- start_info()

    # read file but catch errors in case there are any uncaught ones
    out <-
      try_catch_cnds(
        aggregate_data(
          uidx = uidx,
          datasets = files_data[uidx, ] |> unlist(recursive = FALSE),
          aggregator = aggregator,
          show_problems = FALSE # show them later
        ),
        error_value = list(
          file_info = list(),
          problems = tibble()
        ),
        catch_errors = !orbi_get_option("debug")
      )

    # add filepath to file info
    out$result$file_info <- c(
      # uidx is already in the dataset but faster to just add it in again
      list(uidx = uidx, filepath = filepath),
      out$result$file_info[
        !names(out$result$file_info) %in% c("uidx", "filepath")
      ]
    )

    # merge old, returned, and caught problems
    problems <- bind_rows(out$result$problems, out$conditions) |>
      mutate(new = TRUE)
    out$result$problems <- files_data$problems[[uidx]] |>
      dplyr::bind_rows(problems) |>
      dplyr::mutate(uidx = !!uidx) |>
      dplyr::relocate("uidx", .before = 1L)

    if (show_problems && nrow(problems) > 0) {
      # info
      finish_info(
        format_inline("tried to aggregate {.file {basename(filepath)}}"),
        start = file_start,
        conditions = problems,
        show_conditions = show_problems,
        .call = call,
        .env = root_env
      )
    }

    # return
    return(out$result)
  }

  # read files (separate calls to simplify backtraces)
  results <- files_data$filepath |>
    map2(1:nrow(files_data), aggregate_safely_with_progress)

  # unused columns (this is purely for information purpses)
  fetch_unused_columns <- function(result, dataset) {
    if (dataset %in% names(attr(result, "unused_columns"))) {
      attr(result, "unused_columns")[[dataset]]
    } else {
      character(0)
    }
  }

  # combine results
  datasets <- c(unique(aggregator$dataset), "problems")
  fetch_dataset <- function(result, dataset) {
    if (dataset %in% names(result)) {
      result[[dataset]]
    } else {
      NULL
    }
  }
  results <-
    as.list(datasets) |>
    setNames(datasets) |>
    purrr::map(
      ~ {
        data <- purrr::map(results, fetch_dataset, .x) |>
          dplyr::bind_rows()
        unused_cols <- purrr::map(results, fetch_unused_columns, .x) |>
          unlist(recursive = TRUE) |>
          unique()
        attr(data, "unused_columns") <- unused_cols
        data
      }
    )

  # info
  n_rows <- map_int(results, nrow) |> format_number()
  details <- sprintf("{cli::col_blue('%s')} (%s)", names(results), n_rows) |>
    purrr::map_chr(format_inline)
  new_problems <- results$problems |>
    dplyr::filter(!is.na(.data$new) & .data$new)
  finish_info(
    "aggregated {utils::head(details, -1)} from {nrow(files_data)} file{?s} using the {.emph {.strong {attr(aggregator, 'name')}}} aggregator",
    if (nrow(new_problems) > 0) {
      # custom problems summary
      summarize_cnds(
        new_problems,
        include_symbol = FALSE,
        include_call = FALSE,
        summary_format = "but encountered {issues} {symbol$arrow_right} check with {.strong orbi_get_problems(x)}"
      )
    },
    success_format = if (nrow(new_problems) > 0) {
      "{cli::col_yellow('!')} {msg}"
    } else {
      "{cli::col_green(symbol$tick)} {msg}"
    },
    start = start
  )

  # clean up problems
  results$problems <- dplyr::select(results$problems, -"new")

  # return
  return(results)
}

# actual aggregation function
# note that this function is a lot more universal than just for raw data
# it can be used for any isoverse aggregation and should be considered
# for porting to isoverse
aggregate_data <- function(uidx, datasets, aggregator, show_problems = TRUE) {
  # check which datasets are available
  check_datasets <- function(aggregator) {
    if (length(missing <- setdiff(aggregator$dataset, names(datasets))) > 0) {
      cli_warn(
        "dataset{?s} {.var {missing}} do{?es/} not exist and will be skipped during aggregation"
      )
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

  # find source columns and deal with any regexps that need to be resolved
  # shouldn't throw errors relevant for user so not run with try_catch_cnds
  all_columns <- purrr::map(datasets, names)
  find_source_columns <- function(dataset, column, source, regexp) {
    # find source columns
    available_cols <- all_columns[[dataset]]
    # find by regexp
    if (regexp && length(source) > 0) {
      sources <- grep(
        source[[1]],
        available_cols,
        value = TRUE,
        ignore.case = TRUE,
        perl = TRUE
      )
      if (length(sources) > 0) {
        # found some matching regexps
        columns <- gsub(
          source[[1]],
          column,
          sources,
          ignore.case = TRUE,
          perl = TRUE
        )
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
    dplyr::select(-"column", -"source") |>
    tidyr::unnest("cols") |>
    dplyr::filter(!is.na(.data$column)) |>
    # check for columns from regexps that are already covered by direct use
    dplyr::mutate(
      .by = "dataset",
      regexp_col_only = purrr::map2_lgl(
        .data$source,
        .data$regexp,
        function(source, regexp, non_regexp_sources) {
          regexp && !any(source %in% non_regexp_sources)
        },
        non_regexp_sources = .data$source[!.data$regexp] |>
          unlist(recursive = TRUE)
      )
    ) |>
    dplyr::filter(!.data$regexp | .data$regexp_col_only)

  # all used columns
  used_columns <- aggregator_applied |>
    dplyr::summarize(
      .by = "dataset",
      all_columns = list(all_columns[[.data$dataset[1]]]),
      used_columns = list(.data$source |> unlist(recursive = TRUE) |> unique()),
      unused_columns = list(setdiff(all_columns[[1]], used_columns[[1]]))
    )

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
    default_expr <- rlang::expr((!!cast)(!!default[[1]]))
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
  # note: unnest_wider conflates integer()/numeric() with NULL and can NOT be used here
  aggregator_applied <- aggregator_applied |>
    dplyr::mutate(
      result = map(.data$value, `[[`, 1),
      conditions = map(.data$value, `[[`, 2)
    ) |>
    dplyr::select(-"value")

  # finalize datasets into tibbles
  aggregator_summary <-
    aggregator_applied |>
    filter(!purrr::map_lgl(.data$result, is.null)) |>
    summarize(
      .by = "dataset",
      out = list(.data$result |> setNames(.data$column) |> as_tibble())
    )
  datasets <- aggregator_summary$out |> setNames(aggregator_summary$dataset)

  # add uidx to each tibble
  datasets <- datasets |>
    purrr::map(
      ~ {
        if (nrow(.x) > 0) {
          dplyr::mutate(.x, uidx = !!uidx, .before = 1L)
        } else {
          dplyr::mutate(.x, uidx = integer(0), .before = 1L)
        }
      }
    )

  problems <-
    dplyr::bind_rows(
      datasets$conditions,
      aggregator$conditions,
      aggregator_applied$conditions
    ) |>
    dplyr::mutate(uidx = !!uidx, .before = 1L)

  # return with problems
  result <- c(datasets, list(problems = problems))
  attr(result, "unused_columns ") <- used_columns |>
    dplyr::select("dataset", "unused_columns") |>
    tibble::deframe()
  return(result)
}

# get data from aggregated ===========

# get data from an aggregated list of datasets via inner joins
# @param .ds the list of datasets (uses .ds to avoid accidental partial mapping by ...)
# @param ... named arguments for each dataset that should be included with a tidyselect expression for the columns to include
# @param by which columns to use for join by operations (if there are any)
# @param always_include_by - include the by columns in any dataset where they exist
# @param relationship passed to inner_join, default expectation is one to many
get_data <- function(
  .ds,
  ...,
  by = c(),
  always_include_by = TRUE,
  relationship = "one-to-many",
  .env = caller_env(),
  .call = caller_call()
) {
  # start
  start <- start_info("is running", .call = .call)

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
    cli_abort(
      c(
        "no dataset column selections provided",
        "i" = "available dataset{?s}: {cli::col_blue(names(.ds))}"
      ),
      call = .env
    )
  }

  # valid selectors?
  if (any(missing <- !names(selectors) %in% names(.ds))) {
    cli_abort(
      c(
        "dataset{?s} not in the data: {cli::col_blue(names(selectors)[missing])}",
        "i" = "available dataset{?s}: {cli::col_blue(names(.ds))}"
      ),
      call = .env
    )
  }

  # quick loop (better errors than map for the small number of .ds)
  join_bys <- list()
  out <- tibble::tibble()
  for (i in seq_along(selectors)) {
    # fetch
    new_data <- tryCatch(
      .ds[[names(selectors)[i]]] |>
        dplyr::select(dplyr::any_of(!!by), !!selectors[[i]]),
      error = function(cnd) {
        cli_abort(
          c(
            "encountered an error selecting columns for {cli::col_blue(names(selectors)[i])}",
            "i" = "columns in {cli::col_blue(names(selectors)[i])}: {.field {names(.ds[[names(selectors)[i]]])}}"
          ),
          parent = cnd,
          call = .env
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
              "unclear how to join {cli::col_blue(names(selectors)[i])}",
              "with {cli::col_blue(names(selectors)[1:(i-1)])}"
            ),
            "i" = if (length(by) == 0) {
              "there are no join columns defined in {.var by}"
            } else {
              "allowed join columns: {.field {by}}"
            },
            "i" = "selected columns in {cli::col_blue(names(selectors)[i])}: {.field {names(new_data)}}",
            "i" = "selected columns in {cli::col_blue(names(selectors)[1:(i-1)])}: {.field {names(out)}}"
          ),
          call = .env
        )
      }

      # catch join error/warnings
      catch_error_and_warning <- function(cnd) {
        cli_abort(
          c(
            paste(
              "encountered issue when joining {cli::col_blue(names(selectors)[i])}",
              "with {cli::col_blue(names(selectors)[1:(i-1)])} by {.field {join_by}}",
              "with relationship \"{relationship}\""
            ),
            "i" = "are you sure the by column{?s} ({.field {join_by}}) {?is/are} sufficient and this operation is really what is intended?"
          ),
          parent = cnd,
          call = .env
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
          warning = catch_error_and_warning,
          error = catch_error_and_warning
        )
      join_bys <- c(join_bys, join_by)
    } else {
      # just one dataset
      out <- new_data
    }
  }

  # info
  n_rows <- purrr::map_int(.ds[names(selectors)], nrow) |> format_number()

  details <-
    if (length(selectors) == 1) {
      sprintf("{cli::col_blue('%s')}", names(selectors))
    } else {
      sprintf("{cli::col_blue('%s')} (%s)", names(selectors), n_rows)
    }
  details <- details |> purrr::map_chr(format_inline)

  # info
  finish_info(
    "retrieved {format_number(nrow(out))} records from ",
    if (length(selectors) > 1) "the combination of ",
    "{details}",
    if (length(selectors) > 1) " via {.field {unique(unlist(join_bys))}}",
    start = start,
    .call = .call
    # don't pass parent .env since we want these glues to execute in THIS env
  )

  # return
  return(out)
}
