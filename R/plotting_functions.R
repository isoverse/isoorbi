
# utility functions ========

# internal function to filter for specific isotopocules
filter_isotopocules <- function(dataset, isotopocules, allow_all = TRUE) {
  dataset <- dataset |> factorize_dataset("isotopocules")
  if (allow_all && length(isotopocules) == 0L) 
    isotopocules <- levels(dataset$isotopocule)
  missing_isotopocules <- !isotopocules %in% levels(dataset$isotopocule)
  if (sum(missing_isotopocules) > 0L) {
    sprintf("not all `isotopocules` are in the dataset, missing '%s'. Available: '%s'", 
            paste(isotopocules[missing_isotopocules], collapse = "', '"),
            paste(levels(dataset$isotopocule), collapse = "', '")) |>
      warn()
  }
  isotopocules <- isotopocules[!missing_isotopocules]
  if (length(isotopocules) == 0L)
    abort("none of the provided `isotopocules` are in the dataset")
  
  # plot dataset
  dataset |>
    dplyr::filter(.data$isotopocule %in% isotopocules) |>
    droplevels()
}

# internal function to nicely format log scales
label_scientific_log <- function() {
  parser1 <- scales::label_scientific()
  parser2 <- scales::label_parse()
  parser3 <- scales::label_log()
  function(x) {
    needs_decimal <- any((log10(na.omit(x)) %% 1) > 0)
    if (needs_decimal) {
      out <- x |> 
        parser1() |>
        stringr::str_replace("e\\+?", " %.% 10^") |>
        parser2() 
    } else {
      out <- parser3(x)
    }
    out[x == 0.0] <- 0
    return(out)
  }
}

# y axis as log, pseudo-log, or continuous
dynamic_y_scale <- function(plot,  y_scale = c("none", "continuous", "pseudo-log", "log"), sci_labels = FALSE, breaks = scales::pretty_breaks(5)) {
  y_scale <- arg_match(y_scale)
  labeler <- if(sci_labels) label_scientific_log() else identity
  if (y_scale == "log") {
    plot <- plot + ggplot2::scale_y_log10(label = labeler)
  } else if (y_scale == "pseudo-log") {
    plot <- plot + 
      scale_y_continuous(
        trans = scales::pseudo_log_trans(),
        breaks = breaks,
        labels = labeler
      )
  } else if (y_scale == "continuous"){
    plot <- plot + 
      scale_y_continuous(labels = labeler)
  }
  return(plot)
}

# internal function for the facet_wrap
# decides whether to wrap by filename, compound or both filename and compound
# depending on if either has more than 1 value
dynamic_wrap <- function(plot, scales = "free_x") {
  dataset <- p$data |> factorize_dataset(c("filename", "compound"))
  n_files <- length(levels(dataset$filename))
  n_compounds <- length(levels(dataset$compound))
  if (n_files > 1L && n_compounds > 1L) {
    plot <- plot + ggplot2::facet_wrap(~.data$filename + .data$compound, scales = scales)
  } else if (n_compounds > 1L) {
    plot <- plot + ggplot2::facet_wrap(~.data$compound, scales = scales)
  } else if (n_files > 1L) {
    plot <- plot + ggplot2::facet_wrap(~.data$filename, scales = scales)
  }
  return(plot)
}

#' Default isoorbi plotting theme
#' @return ggplot theme object
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
orbi_calculate_isotopocule_coverage <- function(dataset) {
  
  # safety checks
  cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule", "ions.incremental")
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`" =
      all(cols %in% names(dataset))
  )
  
  # nesting requires global defs
  scan_no <- time.min <- NULL
  
  # calculate coverage
  dataset <- dataset |> factorize_dataset(c("filename", "compound", "isotopocule"))
  isotopocule_levels <- levels(dataset$isotopocule)
  dataset |>
    # complete dataset (need isotopocule as char otherwise will always complete for all levels)
    dplyr::select("filename", "compound", "isotopocule", "scan.no", "time.min", "ions.incremental") |>
    dplyr::mutate(isotopocule = as.character(.data$isotopocule)) |>
    dplyr::group_by(.data$filename, .data$compound) |>
    tidyr::complete(.data$isotopocule, tidyr::nesting(scan.no, time.min)) |>
    dplyr::ungroup() |>
    # find data/no-data stretches
    dplyr::arrange(.data$filename, .data$compound, .data$isotopocule, .data$scan.no) |>
    dplyr::mutate(
      isotopocule = factor(.data$isotopocule, levels = isotopocule_levels),
      group = c(0, cumsum(abs(diff(is.na(.data$ions.incremental))))),
      .by = c("filename", "compound", "isotopocule")
    ) |>
    # summarize
    tidyr::nest(data = c(.data$scan.no, .data$time.min, .data$ions.incremental)) |>
    dplyr::mutate(
      has_data = !map_lgl(.data$data, ~is.na(.x$ions.incremental[1])),
      n_points = map_int(.data$data, nrow),
      start_scan.no = map_dbl(.data$data, ~.x$scan.no[1]),
      end_scan.no = map_dbl(.data$data, ~tail(.x$scan.no, 1)),
      start_time.min = map_dbl(.data$data, ~.x$time.min[1]),
      end_time.min = map_dbl(.data$data, ~tail(.x$time.min, 1))
    ) |>
    dplyr::arrange(.data$filename, .data$compound, .data$isotopocule)
}

# plot functions ==========

#' Visualize satellite peaks
#' 
#' Call this function any time after flagging the satellite peaks to see where they are. Use the `isotopocules` argument to focus on the specific isotopocules of interest.
#' 
#' @param dataset isox dataset with satellite peaks identified (`orbi_flag_satellite_peaks()`)
#' @param isotopocules which isotopocules to visualize, if none provided will visualize all (this may take a long time or even crash your R session if there are too many isotopocules in the data set)
#' @param x x-axis column for the plot, either "time.min" or "scan.no"
#' @param y_scale what type of y scale to use: "log" scale, "pseudo-log" scale (smoothly transitions to linear scale around 0), "continuous" scale or "none" if you want to add a y scale to the plot manually instead
#' @param y_scale_sci_labels whether to render numbers with scientific exponential notation
#' @param colors which colors to use, by default a color-blind friendly color palettes (RColorBrewer, dark2)
#' @export
orbi_plot_satellite_peaks <- function(
    dataset, isotopocules = c(), x = c("scan.no", "time.min"), 
    y_scale = c("log", "pseudo-log", "continuous", "none"), y_scale_sci_labels = TRUE,
    colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")) {
  
  # safety checks
  cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule", "ions.incremental")
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`isotopocules` has to be a character vector if provided" = length(isotopocules) == 0L || is_character(isotopocules),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`" =
      all(cols %in% names(dataset)),
    "`dataset` requires column `is_satellite_peak` - make sure to run `orbi_flag_satellite_peaks()` first" = "is_satellite_peak" %in% names(dataset)
  )
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
      x = !!sym(x_column), y = .data$ions.incremental, 
      color = .data$isotopocule) +
    ggplot2::geom_line(
      data = function(df) dplyr::filter(df, !.data$is_satellite_peak),
      alpha = 0.5
    ) + 
    ggplot2::geom_point(
      data = function(df) dplyr::filter(df, .data$is_satellite_peak) |> 
        dplyr::mutate(flagged = "satellite peaks"), 
      map = aes(shape = .data$flagged)
    ) + 
    ggplot2::scale_shape_manual(values = 17) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(shape = NA), order = 1)
    ) +
    orbi_default_theme()
  
  # return
  plot |> 
    dynamic_y_scale(y_scale, sci_labels = y_scale_sci_labels) |>
    dynamic_wrap()
}


#' Plot isotopocule coverage
#' 
#' @param dataset isox data
#' @inheritParams orbi_plot_satellite_peaks
#' @param add_blocks add blocks if there are any block definitions in the dataset
#' @export
orbi_plot_isotopocule_coverage <- function(
    dataset, isotopocules = c(), x = c("scan.no", "time.min"),
    add_blocks = TRUE
    ) {
  
  # safety checks
  cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule", "ions.incremental")
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`isotopocules` has to be a character vector if provided" = length(isotopocules) == 0L || is_character(isotopocules),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`" =
      all(cols %in% names(dataset))
  )
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
  
  # changes
  files_delta_x <- 
    dataset |> 
    dplyr::group_by(.data$filename) |> 
    dplyr::summarize(
      delta_x = 
        if(x_column == "time.min") 
          (max(.data$time.min) - min(.data$time.min)) / (max(.data$scan.no) - min(.data$scan.no))
        else 1
    )
  
  # calculate coverage
  isotopocule_coverage <- 
    dataset |> 
    orbi_calculate_isotopocule_coverage() |>
    dplyr::mutate(
      y = as.integer(.data$isotopocule),
      xmin = if(x_column == "time.min") .data$start_time.min else .data$start_scan.no,
      xmax = if(x_column == "time.min") .data$end_time.min else .data$end_scan.no,
    ) |>
    dplyr::left_join(files_delta_x, by = "filename")
  
  # outlines (to show which isotopocules are recorded at all)
  scan_outlines <-
    isotopocule_coverage |>
    dplyr::summarize(
      xmin = min(.data$xmin), 
      xmax = max(.data$xmax),
      .by = c("filename", "compound", "y")
    ) |>
    dplyr::left_join(files_delta_x, by = "filename")
  
  # make plot
  plot <- 
    dataset |>
    ggplot2::ggplot() + 
    ggplot2::aes(
      y = .data$y,
      xmin = .data$xmin - .data$delta_x, xmax = .data$xmax + .data$delta_x, 
      ymin = .data$y - 0.4, ymax = .data$y + 0.4
    ) +
    # background outline
    ggplot2::geom_rect(data = scan_outlines, fill = NA, color = "black") +
    # missing data
    ggplot2::geom_rect(
      data = isotopocule_coverage |> dplyr::filter(!.data$has_data),
      fill = "white"
    ) +
    # has data
    ggplot2::geom_rect(
      data = isotopocule_coverage |> dplyr::filter(.data$has_data),
      fill = "black"
    ) +
    scale_y_reverse(
      breaks = seq_along(levels(isotopocule_coverage$isotopocule)), 
      labels = levels(isotopocule_coverage$isotopocule)
    ) +
    orbi_default_theme() +
    labs(x = x_column, y = NULL)
  
  # blocks
  if (add_blocks && has_blocks(dataset))
    plot <- plot |> orbi_add_blocks_to_plot(x = x_column)
  
  # return
  plot |> dynamic_wrap()
}

#' Visualize raw data
#' 
#' Call this function to visualize orbitrap data vs. time or scan number. The most common uses are `orbi_plot_raw_data(y = intensity)`, `orbi_plot_raw_data(y = ratio)`, and `orbi_plot_raw_data(y = tic * it.ms)`.
#' 
#' @param dataset isox dataset
#' @param isotopocules which isotopocule(s) to visualize
#' @param x x-axis for the plot, either "time.min" (the default) or "scan.no"
#' @param y expression for what to plot on the y-axis, e.g. `intensity`, `tic * it.ms`, `ratio`, etc. the default is `intensity`
#' @param color column for the plot, default is "isotopocule"
#' @param colors which colors to use, by default a color-blind friendly color palettes (RColorBrewer, dark2)
#' @param add_blocks whether to show blocks (if any are defined)
#' @export
orbi_plot_raw_data <- function(
    dataset, isotopocules, x = c("time.min", "scan.no"), y = intensity, color = "isotopocule",
    colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
    ) {
  
  # safety checks
  cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule")
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "need at least one value for `isotopocules`" = !missing(isotopocules) && is_character(isotopocules) && length(isotopocules) > 0L,
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`" =
      all(cols %in% names(dataset)),
    "`color` needs to be a column name" = is_scalar_character(color) && color %in% names(dataset)
  )
  x_column <- arg_match(x)
  
  # prepare dataset
  plot_df <- 
    dataset |> 
    factorize_dataset(c("filename", "compound", "isotopocule")) |>
    filter_isotopocules(isotopocules) |>
    # only filter out satellite peaks
    filter_flagged_data(
      filter_satellite_peaks = TRUE,
      filter_weak_isotopocules = FALSE, 
      filter_outliers = FALSE)
  
  # generate y value
  yquo <- enquo(y)
  plot_df <- 
    try_catch_all(
      plot_df |>
        dplyr::mutate(y = !!yquo),
      sprintf("something went wrong generating the `y` variable `%s`",
              rlang::as_label(yquo))
    )
  
  # make plot
  plot <- plot_df |> 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = !!sym(x_column), y = .data$y, 
     color = !!sym(color)) +
    ggplot2::geom_line() + 
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      strip.text = ggplot2::element_text(size = 20),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank()
    ) +
    labs(y = as_label(yquo))
  
  # wrap
  n_files <- length(levels(plot_df$filename))
  n_compounds <- length(levels(plot_df$compound))
  if (n_files > 1L && n_compounds > 1L) {
    plot <- plot + ggplot2::facet_wrap(~.data$filename + .data$compound, scales = "free_x")
  } else if (n_compounds > 1L) {
    plot <- plot + ggplot2::facet_wrap(~.data$compound, scales = "free_x")
  } else if (n_files > 1L) {
    plot <- plot + ggplot2::facet_wrap(~.data$filename, scales = "free_x")
  }
  
  # return
  return(plot)
}