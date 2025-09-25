# utility functions ========

# internal function to filter for specific isotopocules during plotting
filter_isotopocules <- function(
  dataset,
  isotopocules,
  allow_all = TRUE,
  call = caller_env()
) {
  dataset <- dataset |> factorize_dataset("isotopocule")
  if (allow_all && length(isotopocules) == 0L) {
    isotopocules <- levels(dataset$isotopocule)
  }
  missing_isotopocules <- !isotopocules %in% levels(dataset$isotopocule)

  if (sum(!missing_isotopocules) == 0) {
    cli_abort(
      c(
        "none of the provided {.field isotopocules} are in the dataset",
        "i" = "provided: {.val {isotopocules}}",
        "i" = "available: {.val {levels(dataset$isotopocule)}}"
      ),
      call = call
    )
  }

  if (sum(missing_isotopocules) > 0L) {
    cli_alert_warning("not all {.field isotopocules} are in the dataset")
    cli_alert_info(
      "missing (will be ignored): {.val {isotopocules[missing_isotopocules]}}"
    )
    cli_alert_info("available: {.val {levels(dataset$isotopocule)}}")
  }
  isotopocules <- isotopocules[!missing_isotopocules]

  # plot dataset
  dataset |>
    dplyr::filter(.data$isotopocule %in% !!isotopocules) |>
    droplevels()
}

# internal function to nicely format log scales
#' @importFrom stats na.omit
label_scientific_log <- function() {
  parser1 <- scales::label_scientific()
  parser2 <- scales::label_parse()
  parser3 <- scales::label_log()
  function(x) {
    needs_decimal <- any((log10(na.omit(x)) %% 1) > 0)
    if (needs_decimal) {
      parsed_x <- x |>
        parser1()
      out <- sub("e\\+?", " %.% 10^", parsed_x)
      out <- out |>
        parser2()
    } else {
      out <- parser3(x)
    }
    out[x == 0.0] <- 0
    return(out)
  }
}

# y axis as log, pseudo-log, or continuous
dynamic_y_scale <- function(
  plot,
  y_scale = c("raw", "linear", "pseudo-log", "log"),
  sci_labels = FALSE,
  breaks = scales::pretty_breaks(5)
) {
  y_scale <- arg_match(y_scale)
  labeler <- if (sci_labels) label_scientific_log() else identity
  if (y_scale == "log") {
    plot <- plot +
      ggplot2::scale_y_log10(label = labeler) +
      ggplot2::annotation_logticks(sides = "l")
  } else if (y_scale == "pseudo-log") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        trans = scales::pseudo_log_trans(),
        breaks = breaks,
        labels = labeler
      )
  } else if (y_scale == "linear") {
    plot <- plot +
      ggplot2::scale_y_continuous(labels = labeler)
  }
  return(plot)
}

# internal function for the facet_wrap
# decides whether to wrap by filename, compound or both filename and compound
# depending on if either has more than 1 value
dynamic_wrap <- function(plot, scales = "free_x") {
  dataset <- plot$data
  n_files <- 1
  if ("filename" %in% names(dataset)) {
    dataset <- dataset |> factorize_dataset("filename") |> suppressMessages()
    n_files <- length(levels(dataset$filename))
  }
  n_compounds <- 1L
  if ("compound" %in% names(dataset)) {
    dataset <- dataset |> factorize_dataset("compound") |> suppressMessages()
    n_compounds <- length(levels(dataset$compound))
  }
  if (n_files > 1L && n_compounds > 1L) {
    plot <- plot +
      ggplot2::facet_wrap(~ .data$filename + .data$compound, scales = scales)
  } else if (n_compounds > 1L) {
    plot <- plot + ggplot2::facet_wrap(~ .data$compound, scales = scales)
  } else if (n_files > 1L) {
    plot <- plot + ggplot2::facet_wrap(~ .data$filename, scales = scales)
  }
  return(plot)
}

#' Default isoorbi plotting theme
#' @return ggplot theme object
#' @param text_size a font size for text
#' @param facet_text_size a font size for facet text
#' @export
orbi_default_theme <- function(text_size = 16, facet_text_size = 20) {
  ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      strip.text = ggplot2::element_text(size = facet_text_size),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank()
    )
}

#' Calculate isotopocule coverage
#'
#' Calculate which stretches of the data have data for which isotopocules. This function is usually used indicrectly by `orbi_plot_isotopocule_coverage()` but can be called directly to investigate isotopocule coverage.
#'
#' @param dataset A data frame or aggregated dataset (i.e. works both right after [orbi_identify_isotopocules()] or later downstream after [orbi_get_isotopocules()] or when reading from an IsoX file)
#' @return summary data frame
#' @export
orbi_get_isotopocule_coverage <- function(dataset) {
  # safety checks
  check_arg(
    dataset,
    !missing(dataset) &&
      (is(dataset, "orbi_aggregated_data") ||
        is.data.frame(dataset)),
    "must be a set of aggregated raw files or a data frame of peaks"
  )

  # get peaks tibble
  peaks <- if (is(dataset, "orbi_aggregated_data")) dataset$peaks else dataset

  # check columns
  check_tibble(
    peaks,
    c("uidx|filename", "scan.no", "isotopocule", "ions.incremental|intensity"),
    regexps = TRUE
  )

  # y column (only used for filtering)
  y_col <- names(tidyselect::eval_select(
    any_of(c("ions.incremental", "intensity")),
    peaks
  ))[1]

  # prep peaks
  peaks <- peaks |>
    # filter out missing and unidentified
    dplyr::filter(!is.na(.data$isotopocule), !is.na(!!sym(y_col))) |>
    # will only factorize if they exist
    factorize_dataset(c("filename", "compound", "isotopocule")) |>
    suppressMessages()
  isotopocule_levels <- levels(peaks$isotopocule)

  # grouping colums
  by_cols <- tidyselect::eval_select(
    any_of(c(
      "uidx",
      "filename",
      "compound",
      "isotopocule",
      # make sure a data group column is included if it exists
      "data_group",
      # make sure a weak isotopocule column is included if it exists
      "is_weak_isotopocule"
    )),
    peaks
  ) |>
    names()

  # arrange colums
  arrange_cols <- tidyselect::eval_select(
    any_of(c(
      "filename",
      "uidx",
      "compound",
      "isotopocule"
    )),
    peaks
  ) |>
    names()

  # calculate coverage
  output <-
    peaks |>
    # need isotopocule as char otherwise will always complete for all levels
    dplyr::mutate(isotopocule = as.character(.data$isotopocule)) |>
    # find data stretches
    dplyr::arrange(!!!map(c(arrange_cols, "scan.no"), sym)) |>
    dplyr::mutate(
      .by = dplyr::all_of(by_cols),
      # re introduce factor
      isotopocule = factor(.data$isotopocule, levels = isotopocule_levels),
      data_stretch = c(0L, cumsum(diff(.data$scan.no) > 1L)) + 1L,
    ) |>
    # summarize
    dplyr::summarize(
      .by = dplyr::all_of(c(by_cols, "data_stretch")),
      n_points = dplyr::n(),
      start_scan.no = .data$scan.no[1],
      end_scan.no = tail(.data$scan.no, 1),
      start_time.min = if ("time.min" %in% names(peaks)) {
        .data$time.min[1]
      } else {
        list(NULL)
      },
      end_time.min = if ("time.min" %in% names(peaks)) {
        tail(.data$time.min[1])
      } else {
        list(NULL)
      }
    ) |>
    dplyr::arrange(
      !!!map(
        c(arrange_cols, if ("data_group" %in% names(peaks)) "data_group"),
        sym
      )
    )
  if (!"time.min" %in% names(peaks)) {
    output <- output |> dplyr::select(-"start_time.min", -"end_time.min")
  }
  return(output)
}

# plot functions ==========

# FIXME: implement
# @param aggregated_data data aggregated by `orbi_aggregate_raw()` and, optionally, with isotopocules already identified by `orbi_identify_isotopocules()`
# @param mz_min which mz to start the main plot window at. By default include all.
# @param mz_max which mz to end the main plot window at. By default include all.
# @param mz_foci which mz values to focus additiona panels on. Can be approximate (i.e. nominal masses). Typically only suppplied if auto_focus does not do the job.
# @param auto_focus whether to find the isotopocules to focus on automatically. Use `mz_foci` to fine-tune what is shown. Basically searches for the next heighest peaks after the base peak and shows their nearby mass window (including neighboring peaks with similar nominal mass) in additional panels. If there are multiple files (or scans) with different peak distributions, uses foci that are most pronounced across all or most files.
# @param max_cols how many panel columns to show at most (i.e. main window + mz_foci). Use `mz_focus` to specify which ones to show if the `auto_focus` does not do the trick.
# @param max_rows how many panel rows to show at most (i.e. however many scans and files are in the dataset). Will issue a warning
# @param label_peaks whether to label isotopocules with their name and m/z. Is ignored if isotopocules have not yet been identified in the dataset.
# @param label_unknown_peaks whether to also label unknown peaks (i.e. those without isotopocule identification) with their m/z
# @param spectra will be automatically extracted from `aggregated_data` but can be supplied here independently for greater control
# @param peaks will be automatically extracted from `aggregated_data` but can be supplied here independently for greater control
orbi_plot_spectra <- function(
  aggregated_data,
  mz_min = 0,
  mz_max = Inf,
  mz_foci = c(),
  auto_focus = TRUE,
  max_cols = 5,
  max_rows = 5,
  label_peaks = TRUE,
  label_unknown_peaks = TRUE,
  spectra = orbi_get_data(
    aggregated_data,
    file_info = everything(),
    scans = everything(),
    spectra = everything()
  ),
  peaks = orbi_get_data(
    aggregated_data,
    file_info = everything(),
    scans = everything(),
    peaks = everything()
  )
) {
  # safety checks

  # FIXME: check the actual aggregated data class type once it's introduced!
  # or actually probably only check spectra and peaks!
  check_arg(aggregated_data, is.list(aggregated_data))
}

#' Visualize satellite peaks
#'
#' Call this function any time after flagging the satellite peaks to see where they are. Use the `isotopocules` argument to focus on the specific isotopocules of interest.
#'
#' @param dataset a data frame or aggregated dataset with satellite peaks already identified (i.e. after [orbi_flag_satellite_peaks()])
#' @param isotopocules which isotopocules to visualize, if none provided will visualize all (this may take a long time or even crash your R session if there are too many isotopocules in the data set)
#' @param x x-axis column for the plot, either "time.min" or "scan.no", default is "scan.no"
#' @param y y-axis column for the plot, typially either "ions.incremental" or "intensity", default is "ions.incremental" (falls back to "intensity" if "ions.incremental" has not been calculated yet for the provided dataset)
#' @param x_breaks what breaks to use for the x axis, change to make more specifid tickmarks
#' @param y_scale what type of y scale to use: "log" scale, "pseudo-log" scale (smoothly transitions to linear scale around 0), "linear" scale, or "raw" (if you want to add a y scale to the plot manually instead)
#' @param y_scale_sci_labels whether to render numbers with scientific exponential notation
#' @param colors which colors to use, by default a color-blind friendly color palettes (RColorBrewer, dark2)
#' @param color_scale use this parameter to replace the entire color scale rather than just the `colors`
#' @return a ggplot object
#' @export
orbi_plot_satellite_peaks <- function(
  dataset,
  isotopocules = c(),
  x = c("scan.no", "time.min"),
  y = c("ions.incremental", "intensity"),
  x_breaks = scales::breaks_pretty(5),
  y_scale = c("log", "pseudo-log", "linear", "raw"),
  y_scale_sci_labels = TRUE,
  colors = c(
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666"
  ),
  color_scale = scale_color_manual(values = colors)
) {
  # safety checks
  check_arg(
    dataset,
    !missing(dataset) &&
      (is(dataset, "orbi_aggregated_data") ||
        is.data.frame(dataset)),
    "must be a set of aggregated raw files or a data frame of peaks"
  )
  check_arg(
    isotopocules,
    missing(isotopocules) || is_character(isotopocules),
    "must be a character vector"
  )

  # keep track for later
  peaks <- if (is(dataset, "orbi_aggregated_data")) {
    orbi_get_data(dataset, peaks = everything()) |> suppressMessages()
  } else {
    dataset
  }

  # check columns
  check_tibble(
    peaks,
    c(paste(x, collapse = "|"), "isotopocule", paste(y, collapse = "|")),
    regexps = TRUE
  )

  # check satelite peak
  if (!"is_satellite_peak" %in% names(peaks)) {
    cli_abort(
      "{.field dataset} requires column {.field is_satellite_peak} - make sure to run {.strong orbi_flag_satellite_peaks()} first"
    )
  }

  # x_column / y_column y_scale
  x_column <- names(tidyselect::eval_select(any_of(x), peaks))[1]
  y_column <- names(tidyselect::eval_select(any_of(y), peaks))[1]
  y_scale <- arg_match(y_scale)

  # prepare dataset
  plot_df <- peaks |>
    factorize_dataset("isotopocule") |>
    suppressMessages() |>
    filter_isotopocules(isotopocules) |>
    dplyr::filter(!is.na(!!sym(y_column)))

  # make plot
  plot <- plot_df |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = !!sym(x_column),
      y = !!sym(y_column),
      color = .data$isotopocule
    ) +
    ggplot2::geom_line(
      data = function(df) dplyr::filter(df, !.data$is_satellite_peak),
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      data = function(df) {
        dplyr::filter(df, .data$is_satellite_peak) |>
          dplyr::mutate(flagged = "satellite peaks")
      },
      map = ggplot2::aes(shape = .data$flagged)
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks, expand = c(0, 0)) +
    ggplot2::scale_shape_manual(values = 17) +
    {
      {
        color_scale
      }
    } +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(shape = NA), order = 1)
    ) +
    orbi_default_theme()

  # return
  plot |>
    dynamic_y_scale(y_scale, sci_labels = y_scale_sci_labels) |>
    dynamic_wrap()
}

#' Visualize raw data
#'
#' Call this function to visualize orbitrap data vs. time or scan number. The most common uses are `orbi_plot_raw_data(y = intensity)`, `orbi_plot_raw_data(y = ratio)`, and `orbi_plot_raw_data(y = tic * it.ms)`. By default includes all isotopcules that have not been previously identified by `orbi_flag_weak_isotopcules()` (if already called on dataset). To narrow down the isotopocules to show, use the `isotopocule` parameter.
#'
#' @param dataset isox dataset
#' @param y expression for what to plot on the y-axis, e.g. `intensity`, `tic * it.ms` (pick one `isotopocules` as this is identical for different istopocules), `ratio`. Depending on the variable, you may want to adjust the `y_scale` and potentially `y_scale_sci_labels` argument.
#' @param color expression for what to use for the color aesthetic, default is isotopocule
#' @param add_data_blocks add highlight for data blocks if there are any block definitions in the dataset (uses `orbi_add_blocks_to_plot()`). To add blocks manually, set `add_data_blocks = FALSE` and manually call the `orbi_add_blocks_to_plot()` function afterwards.
#' @param add_all_blocks add highlight for all blocks, not just data blocks (equivalent to the `data_only = FALSE` argument in `orbi_add_blocks_to_plot()`)
#' @param show_outliers whether to highlight data previously flagged as outliers by `orbi_flag_outliers()`
#' @inheritParams orbi_plot_satellite_peaks
#' @return a ggplot object
#' @export
orbi_plot_raw_data <- function(
  dataset,
  isotopocules = c(),
  x = c("time.min", "scan.no"),
  x_breaks = scales::breaks_pretty(5),
  y,
  y_scale = c("raw", "linear", "pseudo-log", "log"),
  y_scale_sci_labels = TRUE,
  color = .data$isotopocule,
  colors = c(
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666"
  ),
  color_scale = scale_color_manual(values = colors),
  add_data_blocks = TRUE,
  add_all_blocks = FALSE,
  show_outliers = TRUE
) {
  # safety checks
  ## dataset
  check_tibble(
    dataset,
    req_cols = c("filename", "compound", "scan.no", "time.min", "isotopocule")
  )
  ## y
  check_arg(
    y,
    !missing(y),
    format_inline(
      "can be any expression valid in the data frame, common examples include {.field y = intensity}, {.field y = ratio}, or {.field y = tic * it.ms}"
    ),
    include_type = FALSE
  )
  ## isotopocules
  check_arg(
    isotopocules,
    missing(isotopocules) || is_character(isotopocules),
    "must be a character vector"
  )

  ## x_column / y_scale
  x_column <- arg_match(x)
  y_scale <- arg_match(y_scale)

  # prepare dataset
  plot_df <- dataset |>
    factorize_dataset(c("filename", "compound", "isotopocule")) |>
    filter_isotopocules(isotopocules) |>
    # filter out satellite peaks and weak isotopocules (if isotopocules = c())
    filter_flagged_data(
      filter_satellite_peaks = TRUE,
      filter_weak_isotopocules = length(isotopocules) == 0L,
      filter_outliers = FALSE
    )

  # make sure a data group column is included
  if (!"data_group" %in% names(plot_df)) {
    plot_df <- plot_df |> dplyr::mutate(data_group = NA_integer_)
  }

  # check for outlier column
  if (!"is_outlier" %in% names(plot_df)) {
    plot_df$is_outlier <- FALSE
    plot_df$outlier_type <- NA_character_
  } else if (
    "is_outlier" %in%
      names(plot_df) &&
      show_outliers &&
      !"outlier_type" %in% names(plot_df)
  ) {
    abort(
      "trying to highlight outliers based on `is_outlier` column but `outlier_type` column is missing"
    )
  }
  show_outliers <- show_outliers && any(plot_df$is_outlier)

  # generate y value and color to check if they work
  yquo <- enquo(y)
  colorquo <- enquo(color)
  out <- try_catch_cnds(plot_df |> dplyr::mutate(y = !!yquo))
  abort_cnds(
    out$conditions,
    message = "something went wrong generating the {.field y} variable with {.field {as_label(yquo)}}"
  )

  out <- try_catch_cnds(plot_df |> dplyr::mutate(color = !!colorquo))
  abort_cnds(
    out$conditions,
    message = "something went wrong generating the {.field color} variable with {.field {as_label(colorquo)}}"
  )

  # make plot
  plot <- plot_df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = !!sym(x_column), y = {{ y }}, color = {{ color }}) +
    # data
    ggplot2::geom_line(
      data = function(df) dplyr::filter(df, !.data$is_outlier),
      alpha = if (show_outliers) 0.5 else 1.0,
      map = ggplot2::aes(
        group = paste(
          .data$filename,
          .data$compound,
          .data$isotopocule,
          .data$data_group,
          .data$isotopocule
        )
      )
    ) +
    {
      {
        color_scale
      }
    } +
    ggplot2::scale_x_continuous(breaks = x_breaks, expand = c(0, 0)) +
    orbi_default_theme()

  # scale and dynamic wrap
  plot <- plot |>
    dynamic_y_scale(y_scale, sci_labels = y_scale_sci_labels) |>
    dynamic_wrap()

  # blocks
  if (add_all_blocks && has_blocks(dataset)) {
    plot <- plot |> orbi_add_blocks_to_plot(x = x_column)
  } else if (add_data_blocks && has_blocks(dataset)) {
    plot <- plot |>
      orbi_add_blocks_to_plot(
        x = x_column,
        data_only = TRUE,
        fill_colors = "gray80",
        show.legend = TRUE
      )
  }

  # outliers
  if (show_outliers) {
    plot <- plot +
      ggplot2::geom_point(
        data = function(df) {
          dplyr::filter(df, !!show_outliers & .data$is_outlier)
        },
        map = ggplot2::aes(shape = .data$outlier_type)
      ) +
      # typicall only the first or sometimes first two will be used
      ggplot2::scale_shape_manual(values = c(17, 15, 16, 18)) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(shape = NA, fill = NA),
          order = 1
        )
      ) +
      ggplot2::labs(shape = "flagged outliers")
  }

  return(plot)
}

# FIXME: continue HERE!!
#' Plot isotopocule coverage
#'
#' Weak isotopocules (if previously defined by `orbi_flag_weak_isotopocules()`) are highlighted in the `weak_isotopocules_color`.
#'
#' @param dataset isox data
#' @inheritParams orbi_plot_satellite_peaks
#' @inheritParams orbi_plot_raw_data
#' @return a ggplot object
#' @export
orbi_plot_isotopocule_coverage <- function(
  dataset,
  isotopocules = c(),
  x = c("scan.no", "time.min"),
  x_breaks = scales::breaks_pretty(5),
  add_data_blocks = TRUE
) {
  # safety checks
  ## dataset
  check_tibble(
    dataset,
    req_cols = c(
      "filename",
      "compound",
      "scan.no",
      "time.min",
      "isotopocule",
      "ions.incremental"
    )
  )
  ## isotopocules
  check_arg(
    isotopocules,
    missing(isotopocules) || is_character(isotopocules),
    "must be a character vector"
  )
  ## x_column
  x_column <- arg_match(x)

  # prepare dataset
  dataset <- dataset |>
    factorize_dataset(c("filename", "compound", "isotopocule")) |>
    filter_isotopocules(isotopocules) |>
    # filter out satellite peaks
    filter_flagged_data(
      filter_satellite_peaks = TRUE,
      filter_weak_isotopocules = FALSE,
      filter_outliers = FALSE
    )

  # delta x
  files_delta_x <-
    dataset |>
    dplyr::group_by(.data$filename) |>
    dplyr::summarize(
      delta_x = if (x_column == "time.min") {
        (max(.data$time.min) - min(.data$time.min)) /
          (max(.data$scan.no) - min(.data$scan.no))
      } else {
        1
      }
    )

  # weak isotopocules and data groups
  has_weak_col <- "is_weak_isotopocule" %in% names(dataset)
  has_data_groups <- "data_group" %in% names(dataset)

  # calculate coverage
  isotopocule_coverage <-
    dataset |>
    orbi_get_isotopocule_coverage() |>
    dplyr::mutate(
      y = as.integer(.data$isotopocule),
      xmin = if (x_column == "time.min") {
        .data$start_time.min
      } else {
        .data$start_scan.no
      },
      xmax = if (x_column == "time.min") {
        .data$end_time.min
      } else {
        .data$end_scan.no
      },
    ) |>
    dplyr::left_join(files_delta_x, by = "filename")

  # outlines (to show which isotopocules are recorded at all)
  scan_outlines <-
    dataset |>
    dplyr::mutate(
      xmin = if (x_column == "time.min") {
        min(.data$time.min)
      } else {
        min(.data$scan.no)
      },
      xmax = if (x_column == "time.min") {
        max(.data$time.min)
      } else {
        max(.data$scan.no)
      },
      .by = c("filename")
    ) |>
    dplyr::select("filename", "compound", "isotopocule", "xmin", "xmax") |>
    dplyr::distinct() |>
    dplyr::mutate(y = as.integer(.data$isotopocule)) |>
    dplyr::left_join(files_delta_x, by = "filename")

  # group outlines (for weak isotopocule backgrounds)
  if (has_weak_col) {
    group_outlines <-
      isotopocule_coverage |>
      dplyr::summarize(
        is_weak_isotopocule = any(
          !is.na(.data$is_weak_isotopocule) & .data$is_weak_isotopocule
        ),
        xmin = min(.data$xmin),
        xmax = max(.data$xmax),
        .by = c("filename", "compound", "y", "data_group")
      ) |>
      dplyr::group_by(.data$filename, .data$compound) |>
      tidyr::complete(.data$y, .data$data_group) |>
      dplyr::ungroup() |>
      dplyr::left_join(
        suppressWarnings(dataset |> orbi_get_blocks_info()),
        by = c("filename", "data_group")
      ) |>
      dplyr::mutate(
        is_weak_isotopocule = ifelse(
          !is.na(.data$xmin),
          .data$is_weak_isotopocule,
          TRUE
        ),
        xmin = if (x_column == "time.min") {
          .data$start_time.min
        } else {
          .data$start_scan.no
        },
        xmax = if (x_column == "time.min") {
          .data$end_time.min
        } else {
          .data$end_scan.no
        }
      ) |>
      dplyr::left_join(files_delta_x, by = "filename")
  }

  # make plot
  plot <-
    dataset |>
    ggplot2::ggplot() +
    ggplot2::aes(
      y = .data$y,
      xmin = .data$xmin - .data$delta_x,
      xmax = .data$xmax + .data$delta_x,
      ymin = .data$y - 0.4,
      ymax = .data$y + 0.4
    ) +
    # scan outlines
    ggplot2::geom_rect(
      data = scan_outlines,
      map = ggplot2::aes(fill = "not detected"),
      color = "black"
    )

  # weak isotopcules outlines
  if (has_weak_col) {
    plot <- plot +
      ggplot2::geom_rect(
        data = group_outlines |> filter(.data$is_weak_isotopocule),
        map = ggplot2::aes(fill = "was flagged as weak"),
        color = NA_character_
      )
  }

  # continue plot
  plot <- plot +
    # data
    ggplot2::geom_rect(
      data = isotopocule_coverage,
      map = ggplot2::aes(fill = "isotopocule detected")
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks, expand = c(0, 0)) +
    ggplot2::scale_y_reverse(
      breaks = seq_along(levels(isotopocule_coverage$isotopocule)),
      labels = levels(isotopocule_coverage$isotopocule),
      expand = c(0, 0.2)
    ) +
    orbi_default_theme() +
    ggplot2::labs(x = x_column, y = NULL)

  # blocks
  if (add_data_blocks && has_blocks(dataset)) {
    plot <- plot |>
      orbi_add_blocks_to_plot(
        x = x_column,
        data_only = TRUE,
        fill = "data block",
        fill_scale = scale_fill_manual(
          "legend",
          values = c("#1B9E77", "black", "white", "red")
        ),
        show.legend = TRUE
      )
  } else {
    plot <- plot +
      ggplot2::scale_fill_manual("legend", values = c("black", "white", "red"))
  }

  # return
  plot |> dynamic_wrap()
}
