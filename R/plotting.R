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
  dataset <- plot$data |> factorize_dataset(c("filename", "compound"))
  n_files <- length(levels(dataset$filename))
  n_compounds <- length(levels(dataset$compound))
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
#' @param dataset A data frame or tibble produced from IsoX data
#' @return summary data frame
#' @export
orbi_get_isotopocule_coverage <- function(dataset) {
  # safety checks
  cols <- c(
    "filename",
    "compound",
    "scan.no",
    "time.min",
    "isotopocule",
    "ions.incremental"
  )
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`" = all(
      cols %in% names(dataset)
    )
  )

  # prep dataset
  dataset <- dataset |>
    factorize_dataset(c("filename", "compound", "isotopocule"))
  isotopocule_levels <- levels(dataset$isotopocule)

  # make sure a weak isotopocule column is included
  if (!"is_weak_isotopocule" %in% names(dataset)) {
    dataset <- dataset |> dplyr::mutate(is_weak_isotopocule = NA)
  }

  # make sure a data group column is included
  if (!"data_group" %in% names(dataset)) {
    dataset <- dataset |> dplyr::mutate(data_group = NA_integer_)
  }

  # nesting requires global defs
  scan_no <- time.min <- data_group <- NULL

  # calculate coverage
  dataset |>
    # complete dataset (need isotopocule as char otherwise will always complete for all levels)
    dplyr::select(
      "filename",
      "compound",
      "isotopocule",
      "scan.no",
      "time.min",
      "ions.incremental",
      "data_group",
      "is_weak_isotopocule"
    ) |>
    dplyr::mutate(isotopocule = as.character(.data$isotopocule)) |>
    # find data stretches
    dplyr::arrange(
      .data$filename,
      .data$compound,
      .data$isotopocule,
      .data$scan.no
    ) |>
    dplyr::mutate(
      isotopocule = factor(.data$isotopocule, levels = isotopocule_levels),
      data_stretch = c(0, cumsum(diff(.data$scan.no) > 1L)),
      .by = c("filename", "compound", "isotopocule")
    ) |>
    # summarize
    tidyr::nest(data = c("scan.no", time.min, "ions.incremental")) |>
    dplyr::mutate(
      n_points = map_int(.data$data, nrow),
      start_scan.no = map_dbl(.data$data, ~ .x$scan.no[1]),
      end_scan.no = map_dbl(.data$data, ~ tail(.x$scan.no, 1)),
      start_time.min = map_dbl(.data$data, ~ .x$time.min[1]),
      end_time.min = map_dbl(.data$data, ~ tail(.x$time.min, 1))
    ) |>
    dplyr::arrange(
      .data$filename,
      .data$compound,
      .data$isotopocule,
      .data$data_group
    )
}

# plot functions ==========

#' Visualize satellite peaks
#'
#' Call this function any time after flagging the satellite peaks to see where they are. Use the `isotopocules` argument to focus on the specific isotopocules of interest.
#'
#' @param dataset isox dataset with satellite peaks identified (`orbi_flag_satellite_peaks()`)
#' @param isotopocules which isotopocules to visualize, if none provided will visualize all (this may take a long time or even crash your R session if there are too many isotopocules in the data set)
#' @param x x-axis column for the plot, either "time.min" or "scan.no"
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

  ## satelite peak
  if (!"is_satellite_peak" %in% names(dataset)) {
    cli_abort(
      "{.field dataset} requires column {.field is_satellite_peak} - make sure to run {.fn orbi_flag_satellite_peaks} first"
    )
  }

  ## x_column / y_scale
  x_column <- arg_match(x)
  y_scale <- arg_match(y_scale)

  # prepare dataset
  plot_df <- dataset |>
    factorize_dataset(c("filename", "compound", "isotopocule")) |>
    filter_isotopocules(isotopocules)

  # make plot
  plot <- plot_df |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = !!sym(x_column),
      y = .data$ions.incremental,
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
  try_catch_all(
    plot_df |>
      dplyr::mutate(y = !!yquo),
    sprintf(
      "something went wrong generating the `y` variable with `%s`",
      rlang::as_label(yquo)
    )
  )
  try_catch_all(
    plot_df |>
      dplyr::mutate(color = !!colorquo),
    sprintf(
      "something went wrong generating the `color` variable with `%s`",
      rlang::as_label(colorquo)
    )
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
