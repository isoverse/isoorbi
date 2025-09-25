# peak mapping for isotopulues

#' Identify isotopocules
#'
#' Map the mass spectral peaks to specific isotopocules based on their mass.
#'
#' @param aggregated_data either data aggregated from [orbi_aggregate_raw()] or a straight-up tibble data frame of the peaks (e.g. retrieved via `orbi_get_data(peaks = everything())`).
#' @param isotopocules list of isotopocules to map, can be a data frame/tibble or name of a file to read from (.csv/.tsv/.xlsx are all supported). Required columns are `isotopocule/isotopolog`, `mz/mass`, `tolerance/tolerance [mmu]/tolerance [mDa]`, and `charge/z` (these alternative names for the columns, including uppercase versions, are recognized automatically)
#' @return same object as provided in `aggregated_data` with added columns `isotopocule`, `mzNominal` and `charge` as well as any other additional information columns provided in `isotopocules` (`compound` is a common one that other downstream functions understand)
#' @export
orbi_identify_isotopocules <- function(aggregated_data, isotopocules) {
  # current env
  root_env <- current_env()

  # safety checks
  check_arg(
    aggregated_data,
    !missing(aggregated_data) &&
      (is(aggregated_data, "orbi_aggregated_data") ||
        is.data.frame(aggregated_data)),
    "must be a set of aggregated raw files or a data frame of peaks"
  )
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
        read.csv(isotopocules) |> try_catch_cnds()
      } else if (grepl("tsv$", isotopocules)) {
        read.delim(isotopocules) |> try_catch_cnds()
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

  # check isotopologs tibble for the needed columns
  map_cols_all <- names(isotopocules)
  map_cols_reqs <- c(
    "isotopocule" = "isotopolog|isotopocule|isotopologue",
    "mzNominal" = "mass|m/z|mz",
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
    } else if (length(fits) == 0) {
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
    req_cols = c("uidx", "scan.no", "mzMeasured", "intensity")
  )

  # prepare isotopocules
  isotopocules <- isotopocules |>
    dplyr::rename(!!!map_cols) |>
    dplyr::mutate(
      ..iso_idx = dplyr::row_number(),
      # if tolerance is > 0.1, then it must be in mDa (otherwise it would be a 100 mDa tolerance which is meaningless)
      tolerance = if (median(.data$tolerance) > 0.1) {
        0.001 * .data$tolerance
      } else {
        .data$tolerance
      }
    ) |>
    dplyr::filter(!is.na(.data$mzNominal), !is.na(.data$tolerance))

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
    dplyr::filter(abs(.data$mzNominal - .data$mzMeasured) <= .data$tolerance) |>
    # tag multimatches (one peak matches multiple isotopocules) -> tolerances too large?
    dplyr::mutate(
      .by = "..peak_idx",
      ..n_isotopocules = dplyr::n()
    )

  # check on n_isotopocules
  if (any(found_peaks$..n_isotopocules > 1)) {
    overlaps <- found_peaks |>
      dplyr::filter(.data$..n_isotopocules > 1) |>
      dplyr::select("isotopocule", "mzNominal", "tolerance") |>
      dplyr::distinct() |>
      dplyr::mutate(
        label = sprintf(
          "{.field %s} (%s \u00B1 %s)",
          .data$isotopocule,
          .data$mzNominal,
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
      .by = c("uidx", "scan.no", "..iso_idx"),
      ..n_matches = dplyr::n(),
      ..multimatch = .data$..n_matches > 1
    )

  # unidentified
  unidentified_peaks <- peaks |>
    dplyr::anti_join(found_peaks, by = "..peak_idx")

  # missing
  missing_peaks <- isotopocules |>
    dplyr::filter(.data$..iso_idx %in% unique(found_peaks$..iso_idx)) |>
    dplyr::cross_join(
      peaks |> dplyr::select("uidx", "scan.no") |> dplyr::distinct()
    ) |>
    dplyr::anti_join(found_peaks, by = c("..iso_idx", "uidx", "scan.no"))

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
      if_else(!is.na(.data$mzMeasured), .data$mzMeasured, .data$mzNominal)
    )

  # info
  n_peaks <- nrow(peaks)
  n_identified <- found_peaks$..peak_idx |> unique() |> length()
  n_multimatched <- found_peaks |>
    dplyr::filter(.data$..multimatch) |>
    dplyr::select("..iso_idx", "uidx", "scan.no") |>
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
    "identified {n_identified}/{n_peaks} peaks ({round(100 * n_identified/n_peaks)}%) ",
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
    return(aggregated_data)
  } else {
    # got a plain peaks tibble
    return(all_peaks)
  }
}
