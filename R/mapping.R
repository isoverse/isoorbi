# peak mapping for isotopulues

#' Identify isotopocules
#'
#' Map the mass spectral peaks to specific isotopocules based on their mass.
#'
#' @param aggregated_data either data aggregated from [orbi_aggregate_raw()] or a straight-up tibble data frame of the peaks (e.g. retrieved via `orbi_get_data(peaks = everything())`).
#' @param isotopocules list of isotopocules to map, can be a data frame/tibble or name of a file to read from (.csv/.tsv/.xlsx are all supported).
#' Required columns are `isotopocule/isotopolog`, `mz/mass`, `tolerance/tolerance [mmu]/tolerance [mDa]`, and `charge/z` (these alternative names for the columns, including uppercase versions, are recognized automatically).
#' Optional column: `#compound/compound` as well as any other columns that don't match these others.
#' Character columns in the `isotopocules` table (including `isotopocule` and `compound`) are turned into factors with levels that preserve the order of isotopocules.
#' That means that to change the order of isotopocules in downstream plotting functions, make sure to list them in the order you'd like them presented in.
#' @return same object as provided in `aggregated_data` with added columns `compound` (if provided), `itc_uidx` (introduced unique isotopocule index), `isotopocule`, `mzExact` and `charge` as well as any other additional information columns provided in `isotopocules`. Note that the information about columns that were NOT aggregated in previous steps is purposefully not preserved at this step.
#' @export
orbi_identify_isotopocules <- function(aggregated_data, isotopocules) {
  # current env
  root_env <- current_env()

  # safety checks
  check_dataset_arg(aggregated_data)
  check_arg(
    isotopocules,
    !missing(isotopocules) &&
      ((is_scalar_character(isotopocules) &&
        grepl("\\.(csv|tsv|xlsx)$", isotopocules) &&
        file.exists(isotopocules)) ||
        is.data.frame(isotopocules)),
    "must be the path to a csv/tsv/xlsx file or a data frame of isotopocules"
  )

  # read files
  if (is_scalar_character(isotopocules)) {
    out <-
      if (grepl("csv$", isotopocules)) {
        readr::read_csv(isotopocules, show_col_types = FALSE) |>
          try_catch_cnds()
      } else if (grepl("tsv$", isotopocules)) {
        readr::read_tsv(isotopocules, show_col_types = FALSE) |>
          try_catch_cnds()
      } else if (grepl("xlsx$", isotopocules)) {
        readxl::read_excel(isotopocules) |> try_catch_cnds()
      }
    abort_cnds(
      out$conditions,
      message = format_inline(
        "something went wrong reading {.file {isotopocules}}, try loading it yourself and passing the result to this function"
      ),
      summary_format = "{message}. Encountered {issues}:"
    )
    isotopocules <- out$result
  }

  # turn characters into vectors
  chars <- purrr::map_lgl(isotopocules, is.character)
  if (any(chars)) {
    isotopocules <- isotopocules |>
      factorize_dataset(names(isotopocules)[chars]) |>
      suppressMessages()
  }

  # check isotopologs tibble for the needed columns
  map_cols_all <- names(isotopocules)
  map_cols_reqs <- c(
    "compound" = "\\#compound|compound", # optional
    "isotopocule" = "isotopolog|isotopocule|isotopologue",
    "mzExact" = "mass|m/z|mz",
    "tolerance" = "tolerance|tolerance \\[mmu\\]|tolerance \\[mda\\]",
    "charge" = "charge|z"
  )

  find_col <- function(col, pattern) {
    fits <- grep(
      paste0("^(", pattern, ")$"),
      map_cols_all,
      ignore.case = TRUE,
      value = FALSE
    )
    if (length(fits) > 1) {
      cli_abort(
        c(
          "the {.field {col}} column is ambiguous in the provided {cli::col_blue('isotopocules')}",
          "i" = "found {length(fits)} potential matches: {.field {map_cols_all[fits]}}",
          "i" = "accepted pattern (upper or lower case): {cli::col_magenta(pattern)}"
        ),
        call = root_env
      )
    } else if (length(fits) == 0 && col != "compound") {
      cli_abort(
        c(
          "could not identify {.field {col}} column in the provided {cli::col_blue('isotopocules')}",
          "i" = "available column{?s}: {.field {map_cols_all}}",
          "i" = "accepted pattern (upper or lower case): {cli::col_magenta(pattern)}"
        ),
        call = root_env
      )
    }
    return(map_cols_all[fits])
  }
  # note: mapply provides easier errors than map_chr
  map_cols <- mapply(find_col, names(map_cols_reqs), map_cols_reqs)

  # start timer
  start <- start_info()

  # keep track for later
  is_agg <- is(aggregated_data, "orbi_aggregated_data")
  peaks <- if (is_agg) aggregated_data$peaks else aggregated_data
  check_tibble(
    peaks,
    req_cols = c("uidx", "scan.no", "mzMeasured", "intensity"),
    .arg = "dataset"
  )

  # prepare isotopocules
  isotopocules <- isotopocules |>
    dplyr::rename(!!!map_cols) |>
    dplyr::filter(!is.na(.data$isotopocule)) |>
    # add unique isotopocule id
    dplyr::mutate(itc_uidx = dplyr::row_number(), .before = "isotopocule") |>
    dplyr::mutate(
      # if tolerance is > 0.1, then it must be in mDa (otherwise it would be a 100 mDa tolerance which is meaningless)
      tolerance = if (stats::median(.data$tolerance) > 0.1) {
        0.001 * .data$tolerance
      } else {
        .data$tolerance
      }
    ) |>
    dplyr::filter(!is.na(.data$mzExact), !is.na(.data$tolerance))

  # prerepare peaks (remove previous fitting attempts)
  peaks <- peaks |>
    dplyr::filter(!is.na(.data$mzMeasured)) |>
    dplyr::select(-dplyr::any_of(names(isotopocules))) |>
    dplyr::distinct() |>
    # introduce indices
    dplyr::mutate(..peak_idx = dplyr::row_number())

  # find peaks
  found_peaks <- peaks |>
    dplyr::cross_join(isotopocules) |>
    # filter for matches
    dplyr::filter(abs(.data$mzExact - .data$mzMeasured) <= .data$tolerance) |>
    # tag multimatches (one peak matches multiple isotopocules) -> tolerances too large?
    dplyr::mutate(
      .by = "..peak_idx",
      ..n_isotopocules = dplyr::n()
    )

  # check on n_isotopocules
  if (any(found_peaks$..n_isotopocules > 1)) {
    overlaps <- found_peaks |>
      dplyr::filter(.data$..n_isotopocules > 1) |>
      dplyr::select("isotopocule", "mzExact", "tolerance") |>
      dplyr::distinct() |>
      dplyr::mutate(
        label = sprintf(
          "{.field %s} (%s \u00B1 %s)",
          .data$isotopocule,
          .data$mzExact,
          .data$tolerance
        )
      ) |>
      dplyr::pull(.data$label)
    cli_abort(
      c(
        "some peaks fit multiple isotopocules because of overlapping tolerance intervals, please double-check your input",
        "i" = "overlapping: {purrr::map_chr(overlaps, format_inline)}"
      )
    )
  }

  # any multi-matches?
  found_peaks <- found_peaks |>
    # tag multi-matches (one isotopolog matches multiples peaks) -> satellite peaks
    dplyr::mutate(
      .by = c("uidx", "scan.no", "itc_uidx"),
      ..n_matches = dplyr::n(),
      ..multimatch = .data$..n_matches > 1
    )

  # unidentified
  unidentified_peaks <- peaks |>
    dplyr::anti_join(found_peaks, by = "..peak_idx")

  # missing
  missing_peaks <- isotopocules |>
    dplyr::filter(.data$itc_uidx %in% unique(found_peaks$itc_uidx)) |>
    dplyr::cross_join(
      peaks |> dplyr::select("uidx", "scan.no") |> dplyr::distinct()
    ) |>
    dplyr::anti_join(found_peaks, by = c("itc_uidx", "uidx", "scan.no"))

  # complete
  all_peaks <- found_peaks |>
    # combine with those that have no matches
    dplyr::bind_rows(unidentified_peaks) |>
    # and those that are missing --> coverage
    dplyr::bind_rows(missing_peaks) |>
    # keep track of missing and unidentified
    dplyr::mutate(
      ..missing = is.na(.data$mzMeasured),
      ..unidentified = is.na(.data$isotopocule)
    ) |>
    # arrange
    dplyr::arrange(
      .data$uidx,
      .data$scan.no,
      if_else(!is.na(.data$mzMeasured), .data$mzMeasured, .data$mzExact)
    )

  # info
  n_peaks <- nrow(peaks)
  n_identified <- found_peaks$..peak_idx |> unique() |> length()
  n_multimatched <- found_peaks |>
    dplyr::filter(.data$..multimatch) |>
    dplyr::select("itc_uidx", "uidx", "scan.no") |>
    dplyr::distinct() |>
    nrow()
  multimatched_isos <- found_peaks$isotopocule[found_peaks$..multimatch] |>
    unique()
  n_unidentified <- nrow(unidentified_peaks)
  n_missing <- nrow(missing_peaks)
  missing_isos <- unique(missing_peaks$isotopocule)

  # checks
  multimatch_check <- try_catch_cnds({
    if (n_multimatched > 0) {
      # do it this way for proper alignment
      format_inline(
        "{qty(multimatched_isos)} isotopocule{?s} {.field {multimatched_isos}} match{?es/} multiple peaks in some same scans ({n_multimatched} multi-matched peak{?s} in total) - make sure to run {.strong orbi_flag_satellite_peaks()} and {.strong orbi_plot_satellite_peak()}"
      ) |>
        warn()
    }
  })
  missing_check <- try_catch_cnds({
    if (n_missing > 0) {
      # do it this way for proper alignment
      format_inline(
        "{qty(missing_isos)}isotopocule{?s} {.field {missing_isos}} {?is/are} missing from some scans ({n_missing} missing peak{?s} in total) - make sure to evaluate coverage with e.g. {.strong orbi_plot_isotopocule_coverage()}"
      ) |>
        warn()
    }
  })

  # finish info
  finish_info(
    "identified {format_number(n_identified)}/{format_number(n_peaks)} peaks ({signif(100 * n_identified/n_peaks, 2)}%) ",
    "as isotopcules {.field {unique(found_peaks$isotopocule)}}",
    conditions = dplyr::bind_rows(
      multimatch_check$conditions,
      missing_check$conditions
    ),
    start = start
  )

  # cleanup
  all_peaks <- all_peaks |> dplyr::select(-"tolerance", -starts_with(".."))
  if (is_agg) {
    # got aggregated data to begin with --> return aggregated data
    aggregated_data$peaks <- all_peaks
    # remove unused columns info at this stage
    for (name in names(aggregated_data)) {
      attr(aggregated_data[[name]], "unused_columns") <- NULL
    }
    return(aggregated_data)
  } else {
    # got a plain peaks tibble
    attr(all_peaks, "unused_columns") <- NULL
    return(all_peaks)
  }
}


#' Filter isotopocules
#'
#' This function helps filter out missing isotopcules, unidentified peaks, or select for specific isotopocule.
#' It can be called any time after [orbi_identify_isotopocules()] or after reading from an isox file.
#' By default (i.e. if run without setting any parameters), it removes unidentified peaks and missing isotopcules and keeps all others.
#'
#' @inheritParams orbi_flag_satellite_peaks
#' @param isotopocules if provided, only these isotopocules will be kept
#' @param keep_missing whether to keep missing isotopocules in the peaks list (i.e. those that should be there but are not), default is not to keep them
#' @param keep_unidentified whether to keep unidentified isotopocules in the peaks list (i.e. peaks that have not been identified as a specificic isotopocule), default is not to keep them
#' @export
#' @return the `dataset` but filtered for these isotopocules
orbi_filter_isotopocules <- function(
  dataset,
  isotopocules = c(),
  keep_missing = FALSE,
  keep_unidentified = FALSE
) {
  # safety checks
  check_dataset_arg(dataset)

  # get peaks
  is_agg <- is(dataset, "orbi_aggregated_data")
  peaks <- if (is_agg) dataset$peaks else dataset

  # check columns
  check_tibble(peaks, "isotopocule", .arg = "dataset")

  # info
  start <- start_info()
  n_peaks <- nrow(peaks)
  n_unidentified <- 0
  n_missing <- 0
  n_nonspecific <- 0

  # filter out unidentified?
  if (!keep_unidentified) {
    peaks <- peaks |>
      dplyr::filter(!is.na(.data$isotopocule)) |>
      droplevels()
    n_unidentified <- n_peaks - nrow(peaks)
  }

  # filter out missing?
  if (!keep_missing) {
    check_tibble(
      peaks,
      "ions.incremental|intensity",
      regexps = TRUE,
      .arg = "dataset"
    )
    y <- names(tidyselect::eval_select(
      any_of(c("ions.incremental", "intensity")),
      peaks
    ))[1]
    peaks <- peaks |> dplyr::filter(!is.na(!!sym(y))) |> droplevels()
    n_missing <- n_peaks - nrow(peaks) - n_unidentified
  }

  # search for specific isotopocules
  if (!is_empty(isotopocules)) {
    check_arg(
      isotopocules,
      is_character(isotopocules),
      "must be a character vector of isotopocules"
    )
    available_isotopocules <- if (is.factor(peaks$isotopocule)) {
      levels(peaks$isotopocule)
    } else {
      unique(peaks$isotopocule)
    }

    missing_isotopocules <- !isotopocules %in% available_isotopocules

    if (sum(!missing_isotopocules) == 0) {
      cli_abort(
        c(
          "none of the provided {.field isotopocules} are in the dataset",
          "i" = "provided: {.val {isotopocules}}",
          "i" = "available: {.val {available_isotopocules}}"
        )
      )
    }

    if (sum(missing_isotopocules) > 0L) {
      cli_bullets(
        c(
          "!" = "not all requested {.field isotopocules} are in the dataset",
          "i" = "missing (will be ignored): {cli::col_yellow(isotopocules[missing_isotopocules])}",
          "i" = "available: {.field {available_isotopocules}}"
        )
      )
    }
    isotopocules <- isotopocules[!missing_isotopocules]

    # filter for the specific isotopcules
    peaks <- peaks |>
      dplyr::filter(.data$isotopocule %in% !!isotopocules) |>
      droplevels()
    n_nonspecific <- n_peaks - nrow(peaks) - n_unidentified - n_missing
  }

  # info
  info <- c()
  if (n_missing > 0) {
    info <- "{cli::col_yellow('missing')} isotopocules ({format_number(n_missing)})"
  }
  if (n_unidentified > 0) {
    info <- c(
      info,
      "{cli::col_yellow('unidentified')} peaks ({format_number(n_unidentified)})"
    )
  }
  if (n_nonspecific > 0) {
    info <- c(
      info,
      "{qty(isotopocules)}{cli::col_yellow('not')} {?the/one of the} {cli::col_yellow('selected')} isotopocule{?s} {.field {isotopocules}} ({format_number(n_nonspecific)})"
    )
  }
  finish_info(
    if (n_peaks == nrow(peaks)) {
      "kept all peaks because none fit the criteria for removal"
    } else {
      "removed {format_number(n_peaks - nrow(peaks))} / {format_number(n_peaks)} peaks ({signif(100 * (n_peaks - nrow(peaks))/n_peaks, 2)}%) because they were "
    },
    glue::glue_collapse(info, sep = ", ", last = ", or "),
    ". ",
    if (n_peaks != nrow(peaks) && n_nonspecific == 0) {
      "Remaining isotopocules: {.field {unique(peaks$isotopocule)}}."
    },
    start = start
  )

  # return
  if (is_agg) {
    # got aggregated data to begin with --> return aggregated data
    dataset$peaks <- peaks
    return(dataset)
  } else {
    # got a plain peaks tibble
    return(peaks)
  }
}
