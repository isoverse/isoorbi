# utility functions ========

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

# plot functions ==========

#' Plot mass spectra
#'
#' This function visualizes mass spectra from aggregated raw file data.
#' The spectra have to be be previously read in with `include_spectra = c(1, 10, 100)` in [orbi_read_raw()].
#' By default, this function tries to visualize different isotopcule ranges (monoisotopic peak, M+1, M+2, M+3).
#' To focus only on isotopcules of interest, run [orbi_identify_isotopocules()] and [orbi_filter_isotopocules()] first.
#'
#' @param aggregated_data data aggregated by [orbi_aggregate_raw()] and, optionally, with isotopocules already identified by [orbi_identify_isotopocules()], and (also optionally), alreadty filtered with [orbi_filter_isotopocules()]
#' @param mz_min which mz to start the main plot window at. By default include all.
#' @param mz_max which mz to end the main plot window at. By default include all.
#' @param mz_base_peak where is the base peak at (approximately)?. If not specified (the default), takes the largest peak in the `mz_min` to `mz_max` window.
#' @param mz_focus_nominal_offsets which panels to visualize? 0 = whole spectrum, 1 = spectrum around monoisotopic peak + 1 mu (M+1), 2 = M+2, etc.
#' By default includes the whole spectrum and up to +1, +2, +3, and +4 peaks (if they exist).
#' To visualize only the whole spectrum, use `mz_focus_nominal_offsets = 0`.
#' Likewise, to visualize only the area around the monoisotopic peak +1, provide `mz_focus_monimal_offsets = 1` (or `= c(1, 2)` for both +1 and +2 windows).
#' @param max_scans spectra from how many scans to show at most. By default up to 6 (the number of available linetypes). To show only the spectrum from a single scan, set `max_scans = 1`. If more than 6 scan spectra are allowed (and there are more than 6 loaded in the `aggregated_data`), turns of the linetype aesthetic.
#' @param max_files spectra from how many files to show at most. Each file is shown as an additional line of panels.
#' @param label_peaks whether to label the peaks in the M+1/2/3 panels. If isotopcules are already identified from [orbi_identify_isotopocules()], uses the isotopcule names, otherwise the m/z values. Peaks that are missing (identified by [orbi_identify_isotopocules()]) in all spectra are highlighted in red.
#' To avoid showing unidentified/missing peaks, run [orbi_filter_isotopocules()] first.
#' @param show_filenames whether to show the filename in the first panel of reach row (usually the full spectrum panel)
#' @param show_ref_and_lock_peaks whether to show reference and lock mass peaks in the spectrum
#' @param show_focus_backgrounds whether to highlight the M+x panels with specific background colors that match them with the mass bands highlighted in the first panel
#' @param background_colors the colors to use for the background highlighting
#' @export
orbi_plot_spectra <- function(
  aggregated_data,
  mz_min = 0,
  mz_max = Inf,
  mz_base_peak = NULL,
  mz_focus_nominal_offsets = 0:4,
  max_scans = 6,
  max_files = 4,
  label_peaks = TRUE,
  show_filenames = TRUE,
  show_ref_and_lock_peaks = TRUE,
  show_focus_backgrounds = TRUE,
  background_colors = c(
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666",
    "#BBBBBB"
  )
) {
  # safety checks
  check_arg(
    aggregated_data,
    !missing(aggregated_data) && is(aggregated_data, "orbi_aggregated_data"),
    "must be a set of aggregated raw files"
  )
  check_arg(mz_min, is_scalar_double(mz_min), "must be a single number")
  check_arg(mz_max, is_scalar_double(mz_max), "must be a single number")
  check_arg(
    mz_base_peak,
    is.null(mz_base_peak) || is_scalar_double(mz_base_peak),
    "must be a single number if provided"
  )
  check_arg(
    mz_focus_nominal_offsets,
    is_integerish(mz_focus_nominal_offsets) &&
      length(mz_focus_nominal_offsets) > 0 &&
      all(mz_focus_nominal_offsets >= 0),
    "must be a vector of positive integers c(0, 1, 2, etc.)"
  )
  check_arg(
    max_scans,
    is_scalar_integerish(max_scans) && max_scans > 0,
    "must be a positive integer"
  )
  check_arg(
    max_files,
    is_scalar_integerish(max_files) && max_files > 0,
    "must be a positive integer"
  )
  check_arg(
    label_peaks,
    is_scalar_logical(label_peaks),
    "must be TRUE or FALSE"
  )
  check_arg(
    show_filenames,
    is_scalar_logical(show_filenames),
    "must be TRUE or FALSE"
  )
  check_arg(
    show_ref_and_lock_peaks,
    is_scalar_logical(show_ref_and_lock_peaks),
    "must be TRUE or FALSE"
  )
  check_arg(
    show_focus_backgrounds,
    is_scalar_logical(show_focus_backgrounds),
    "must be TRUE or FALSE"
  )
  check_arg(
    background_colors,
    is_character(background_colors) &&
      (!show_focus_backgrounds ||
        length(background_colors) >= length(mz_focus_nominal_offsets - 1)),
    format_inline(
      "must be a character vector with at least {length(mz_focus_nominal_offsets - 1)} colors"
    ),
    include_type = FALSE
  )

  # any spectra
  if (nrow(aggregated_data$spectra) == 0) {
    cli_text(
      "{cli::col_yellow('!')} {.strong Warning}: there are no {.field spectra} in the data, make sure to include them when reading the raw files e.g. with {.strong orbi_read_raw(include_spectra = c(1, 10, 100))}"
    )
  }

  # which scans to consider? warn if there are too many scans
  scans <- unique(aggregated_data$spectra$scan.no)
  if (length(scans) > max_scans) {
    cli_bullets(c(
      "i" = "{cli::col_blue('Info')}: there are {length(scans)} {.field scan{?s}} in this dataset, only the first {if (max_scans > 1) max_scans} will be visualized. To change this, set the {.field max_scans} argument."
    ))
    scans <- scans[1:max_scans]
  }

  # what are the files? warn if there are too many files
  files <- unique(aggregated_data$spectra$uidx)
  if (length(files) > max_files) {
    cli_bullets(c(
      "i" = "{cli::col_blue('Info')}: there are {length(files)} {.field file{?s}} in this dataset, only the first {if (max_files > 1) max_files} will be visualized. To change this, set the {.field max_files} argument."
    ))
    files <- files[1:max_files]
  }

  spectra <- aggregated_data$spectra |>
    dplyr::filter(.data$scan.no %in% !!scans, .data$uidx %in% !!files)

  # add this so it's easier to process the peaks
  if (!"isotopocule" %in% names(aggregated_data$peaks)) {
    aggregated_data$peaks$mzExact <- NA_real_
    aggregated_data$peaks$isotopocule <- NA_character_
  }

  # focus on peaks in the range of interest
  peaks <- aggregated_data$peaks |>
    dplyr::filter(.data$scan.no %in% !!scans, .data$uidx %in% !!files) |>
    dplyr::mutate(
      mzEffective = if_else(
        !is.na(.data$mzMeasured),
        .data$mzMeasured,
        .data$mzExact
      )
    ) |>
    dplyr::filter(.data$mzEffective >= !!mz_min, .data$mzEffective <= !!mz_max)

  # add ref peaks / lock peaks to spectra
  if (show_ref_and_lock_peaks) {
    spectra <- spectra |>
      dplyr::bind_rows(
        peaks |>
          dplyr::filter(
            !is.na(.data$intensity) & .data$isRefPeak | .data$isLockPeak
          ) |>
          dplyr::select("uidx", "scan.no", "mz" = "mzMeasured", "intensity")
      )
  }

  # find nominal offset
  if (is.null(mz_base_peak)) {
    # find largest
    peaks_w_offset <- peaks |>
      # find main peaks across the files and scans
      dplyr::mutate(
        .by = c("uidx", "scan.no"),
        main_peak = !is.na(.data$intensity) &
          .data$intensity == max(.data$intensity),
        mz_main_peak = .data$mzEffective[.data$main_peak][1]
      ) |>
      # find global main peak (closest to median from all files and scans)
      dplyr::mutate(
        mz_median_diff = abs(
          .data$mz_main_peak - stats::median(.data$mz_main_peak)
        ),
        mz_main_peak = .data$mz_main_peak[
          .data$mz_median_diff == min(.data$mz_median_diff)
        ][1],
        mz_nominal_offset = round(.data$mzEffective - .data$mz_main_peak) |>
          as.integer()
      ) |>
      dplyr::select(-"mz_main_peak", -"mz_median_diff")
  } else {
    # use the provided mz_base_peak as closest
    peaks_w_offset <- peaks |>
      dplyr::mutate(
        mz_nominal_offset = round(.data$mzEffective - !!mz_base_peak) |>
          as.integer()
      )
  }

  # determine offset mass windows
  mz_offset_windows <-
    dplyr::bind_rows(
      dplyr::tibble(
        mz_nominal_offset = 0L,
        mz_min = !!mz_min,
        mz_max = !!mz_max
      ),
      if (any(peaks_w_offset$mz_nominal_offset > 0)) {
        peaks_w_offset |>
          dplyr::filter(.data$mz_nominal_offset > 0) |>
          dplyr::summarize(
            .by = "mz_nominal_offset",
            # expand peak sizes by using the resolution
            mz_min = min(.data$mzEffective) -
              5 * max(.data$mzEffective / .data$peakResolution),
            mz_max = max(.data$mzEffective) +
              5 * max(.data$mzEffective / .data$peakResolution)
          )
      }
    ) |>
    dplyr::filter(
      .data$mz_nominal_offset %in% as.integer(mz_focus_nominal_offsets)
    )

  if (nrow(mz_offset_windows) == 0) {
    cli_abort(
      "none of the requested {.field mz_focus_nominal_offsets} ({mz_focus_nominal_offsets}) exist in this dataset"
    )
  }

  # create plot data frame
  plot_df <- spectra |>
    dplyr::cross_join(mz_offset_windows) |>
    dplyr::filter(.data$mz >= .data$mz_min, .data$mz <= .data$mz_max)

  # start plot
  plot <- plot_df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data$mz, y = .data$intensity)

  # should we use scans as linetype aesthetic?
  if (length(scans) > 1 && length(scans) <= 6) {
    plot <- plot +
      ggplot2::aes(linetype = factor(.data$scan.no)) +
      ggplot2::labs(linetype = "scan")
  } else {
    plot <- plot + ggplot2::aes(group = factor(.data$scan.no))
  }

  # background coloration for the offset foci
  if (
    show_focus_backgrounds &&
      nrow(mz_offset_windows) > 1 &&
      0L %in% mz_offset_windows$mz_nominal_offset
  ) {
    highlights <- mz_offset_windows |>
      dplyr::filter(.data$mz_nominal_offset > 0) |>
      dplyr::mutate(
        fill = background_colors[1:(nrow(mz_offset_windows) - 1)]
      )

    plot <- plot +
      # backgrounds in the base peak window
      ggplot2::geom_rect(
        data = highlights |> dplyr::mutate(mz_nominal_offset = 0L),
        mapping = ggplot2::aes(
          xmin = .data$mz_min - 0.1,
          xmax = .data$mz_max + 0.1,
          ymin = -Inf,
          ymax = +Inf,
          fill = .data$fill
        ),
        alpha = 0.2,
        inherit.aes = FALSE
      ) +
      # backgrounds in the +x panels
      ggplot2::geom_rect(
        data = highlights,
        mapping = ggplot2::aes(
          xmin = -Inf,
          xmax = +Inf,
          ymin = -Inf,
          ymax = +Inf,
          fill = .data$fill
        ),
        alpha = 0.2,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_identity()
  }

  # main parts of plot
  plot <- plot +
    # spectral data
    ggplot2::geom_line() +
    # wrap by windows and file (one file per row)
    ggplot2::facet_wrap(
      ~ .data$uidx + .data$mz_nominal_offset,
      ncol = nrow(mz_offset_windows),
      scales = "free"
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(
        if (length(mz_focus_nominal_offsets) > 1) 3 else 5
      ),
      expand = ggplot2::expansion(mult = c(0, 0.15)),
      labels = label_scientific_log()
    )

  # use fewer x-axis ticks if we have multiple panels
  if (length(mz_focus_nominal_offsets) > 1) {
    plot <- plot +
      ggplot2::scale_x_continuous(
        breaks = scales::pretty_breaks(3),
        expand = c(0, 0.01)
      )
  }

  plot <- plot +
    # make the text size a bit smaller as there is a lot going on on these plots
    orbi_default_theme(text_size = 12) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "m/z")

  # show file names?
  if (show_filenames) {
    plot <- plot +
      ggplot2::geom_text(
        data = aggregated_data$file_info |>
          dplyr::filter(.data$uidx %in% !!files) |>
          dplyr::mutate(
            mz_nominal_offset = min(mz_offset_windows$mz_nominal_offset)
          ),
        map = ggplot2::aes(label = .data$filename),
        x = -Inf,
        y = Inf,
        hjust = -0.03,
        vjust = 1.5,
        inherit.aes = FALSE
      )
  }

  # label peaks
  if (label_peaks && length(mz_focus_nominal_offsets) > 1) {
    plot_peaks <- peaks |>
      dplyr::cross_join(
        mz_offset_windows |> dplyr::filter(.data$mz_nominal_offset > 0)
      ) |>
      dplyr::filter(
        .data$mzEffective >= .data$mz_min,
        .data$mzEffective <= .data$mz_max
      ) |>
      dplyr::mutate(
        label = dplyr::if_else(
          is.na(.data$isotopocule),
          sprintf("%.4f", .data$mzMeasured),
          .data$isotopocule
        ),
        is_missing = is.na(.data$intensity)
      )
    if (nrow(plot_peaks) > 0) {
      plot_peaks <- plot_peaks |>
        # get the scan with the highest intensities for y position labels
        dplyr::mutate(
          .by = c("uidx", "scan.no"),
          scan_intensity = sum(.data$intensity, na.rm = TRUE)
        ) |>
        dplyr::arrange(dplyr::desc(.data$scan_intensity)) |>
        dplyr::filter(
          .by = "uidx",
          .data$scan.no ==
            .data$scan.no[
              .data$scan_intensity == max(.data$scan_intensity)
            ][1]
        )
    }

    # non-missing
    if (any(!plot_peaks$is_missing)) {
      plot <- plot +
        ggplot2::geom_label(
          data = plot_peaks |> dplyr::filter(!.data$is_missing),
          map = ggplot2::aes(
            x = .data$mzEffective,
            y = .data$intensity,
            label = .data$label
          ),
          inherit.aes = FALSE,
          vjust = -0.15
        )
    }
    if (any(plot_peaks$is_missing)) {
      plot <- plot +
        ggplot2::geom_label(
          data = plot_peaks |>
            dplyr::filter(.data$is_missing),
          map = ggplot2::aes(
            x = .data$mzEffective,
            y = 0,
            label = paste0(.data$label, "?")
          ),
          inherit.aes = FALSE,
          fill = "lightpink",
          vjust = -0.15
        )
    }
  }
  # return
  return(plot)
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
    "#666666",
    "#BBBBBB"
  ),
  color_scale = scale_color_manual(values = colors)
) {
  # safety checks
  check_dataset_arg(dataset)

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
    orbi_filter_isotopocules(isotopocules) |>
    suppressMessages() |>
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

#' Visualize data
#'
#' Call this function to visualize orbitrap data vs. time or scan number. The most common uses are `orbi_plot_raw_data(y = intensity)`, `orbi_plot_raw_data(y = ratio)`, and `orbi_plot_raw_data(y = tic * it.ms)`.
#' If the selected `y` is peak-specific data (rather than scan-specific data like `tic * it.ms`), the `isotopocules` argument can be used to narrow down which isotopocules will be plotted.
#' By default includes all isotopcules that have not been previously identified by `orbi_flag_weak_isotopcules()` (if already called on dataset).
#'
#' @inheritParams orbi_flag_satellite_peaks
#' @param y expression for what to plot on the y-axis, e.g. `intensity`, `tic * it.ms` (pick one `isotopocules` as this is identical for different istopocules), `ratio`. Depending on the variable, you may want to adjust the `y_scale` and potentially `y_scale_sci_labels` argument.
#' @param color expression for what to use for the color aesthetic, default is isotopocule
#' @param add_data_blocks add highlight for data blocks if there are any block definitions in the dataset (uses [orbi_add_blocks_to_plot()]). To add blocks manually, set `add_data_blocks = FALSE` and manually call the `orbi_add_blocks_to_plot()` function afterwards.
#' @param add_all_blocks add highlight for all blocks, not just data blocks (equivalent to the `data_only = FALSE` argument in [orbi_add_blocks_to_plot()])
#' @param show_outliers whether to highlight data previously flagged as outliers by [orbi_flag_outliers()]
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
    "#666666",
    "#BBBBBB"
  ),
  color_scale = scale_color_manual(values = colors),
  add_data_blocks = TRUE,
  add_all_blocks = FALSE,
  show_outliers = TRUE
) {
  # safety checks
  ## dataset
  check_dataset_arg(dataset)

  ## y
  check_arg(
    y,
    !missing(y),
    format_inline(
      "can be any expression valid for scans or isotopocules plotting, common examples include {.field y = intensity}, {.field y = ratio}, or {.field y = tic * it.ms}"
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

  # prepare plot_df with y value
  yquo <- enquo(y)
  colorquo <- enquo(color)
  if (is(dataset, "orbi_aggregated_data")) {
    scans <- dataset$scans |>
      dplyr::left_join(
        # make sure to include only file info columns that don't overlap with the scans
        dataset$file_info[
          setdiff(
            names(dataset$file_info),
            names(dataset$scans)
          ) |>
            c("uidx")
        ],
        by = "uidx"
      )
    # see if expression can be evaluated for scans
    out <- try_catch_cnds(scans |> dplyr::mutate(y = !!yquo))
    if (nrow(out$conditions) > 0) {
      # didn't work --> try peaks
      dataset <- dataset |>
        orbi_filter_isotopocules(isotopocules) |>
        suppressMessages()
      plot_df <- dataset$peaks |>
        dplyr::left_join(
          # make sure to include only scan columns that don't overlap with the scans
          scans[
            setdiff(
              names(scans),
              names(dataset$peaks)
            ) |>
              c("uidx", "scan.no")
          ],
          by = c("uidx", "scan.no")
        )
      # safetey check
      out <- try_catch_cnds(plot_df[1, ] |> dplyr::mutate(y = !!yquo))
      abort_cnds(
        out$conditions,
        message = "something went wrong generating the {.field y} variable with {.field {as_label(yquo)}}"
      )
    } else {
      # can evaluate y with scans! use it
      plot_df <- scans
      # check on color too
      out <- try_catch_cnds(plot_df[1, ] |> dplyr::mutate(color = !!colorquo))
      if (nrow(out$conditions) > 0) {
        # --> don't use color quo
        colorquo <- quo(NULL)
      }
    }
  } else {
    # direct dataset
    plot_df <- dataset |>
      orbi_filter_isotopocules(isotopocules) |>
      suppressMessages()
    # safety check
    out <- try_catch_cnds(plot_df[1, ] |> dplyr::mutate(y = !!yquo))
    abort_cnds(
      out$conditions,
      message = "something went wrong generating the {.field y} variable with {.field {as_label(yquo)}}"
    )
  }

  # check plot df
  check_tibble(
    plot_df,
    req_cols = c("filename", "scan.no", "time.min"),
    .arg = "dataset"
  )

  # continue plot_df prep
  plot_df <- plot_df |>
    factorize_dataset(c("filename", "compound", "isotopocule")) |>
    suppressMessages() |>
    # filter out satellite peaks and weak isotopocules (if isotopocules = c())
    filter_flagged_data(
      filter_satellite_peaks = TRUE,
      filter_weak_isotopocules = length(isotopocules) == 0L,
      filter_outliers = FALSE
    ) |>
    droplevels()

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

  # generate color column
  out <- try_catch_cnds(plot_df[1, ] |> dplyr::mutate(color = !!colorquo))
  abort_cnds(
    out$conditions,
    message = "something went wrong generating the {.field color} variable with {.field {as_label(colorquo)}}"
  )

  # make plot
  plot <- plot_df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = !!sym(x_column), y = {{ y }})
  if (!quo_is_null(colorquo)) {
    plot <- plot + ggplot2::aes(color = {{ color }})
  }

  # data
  plot <- plot +
    ggplot2::geom_line(
      data = function(df) dplyr::filter(df, !.data$is_outlier),
      alpha = if (show_outliers) 0.5 else 1.0,
      map = ggplot2::aes(
        group = paste(
          .data$filename,
          .data$data_group,
          if ("compound" %in% names(plot_df)) .data$compound,
          if ("isotopocule" %in% names(plot_df)) .data$isotopocule
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
  if (add_all_blocks && has_blocks(plot_df)) {
    plot <- plot |> orbi_add_blocks_to_plot(x = x_column)
  } else if (add_data_blocks && has_blocks(plot_df)) {
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

#' @inheritParams orbi_plot_satellite_peaks
#' @inheritParams orbi_plot_raw_data
#' @describeIn orbi_isotopocule_coverage visualizes isotope coverage. Weak isotopocules (if previously defined by [orbi_flag_weak_isotopocules()]) are highlighted in red.
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
  check_dataset_arg(dataset)
  x_column <- arg_match(x)

  # filter for isotopocules
  dataset <- dataset |>
    orbi_filter_isotopocules(isotopocules) |>
    suppressMessages()

  # get peaks df
  peaks <- if (is(dataset, "orbi_aggregated_data")) {
    dataset$peaks |>
      dplyr::left_join(
        dataset$file_info |> dplyr::select("uidx", "filename"),
        by = "uidx"
      ) |>
      dplyr::left_join(
        dataset$scans |>
          dplyr::select(
            "uidx",
            "scan.no",
            "time.min",
            dplyr::any_of(c("block", "data_group", "sample_name", "data_type"))
          ),
        by = c("uidx", "scan.no")
      )
  } else {
    dataset
  }

  # check columns
  check_tibble(
    peaks,
    c(
      "uidx|filename",
      "scan.no",
      x_column,
      "isotopocule",
      "ions.incremental|intensity"
    ) |>
      unique(),
    regexps = TRUE,
    .arg = "dataset"
  )

  # prepare dataset
  peaks <- peaks |>
    # factorize isotopocules
    factorize_dataset(c("filename", "isotopocule")) |>
    suppressMessages() |>
    # filter out satellite peaks
    filter_flagged_data(
      filter_satellite_peaks = TRUE,
      filter_weak_isotopocules = FALSE,
      filter_outliers = FALSE
    )

  # grouping colums
  by_cols <- tidyselect::eval_select(any_of(c("uidx", "filename")), peaks) |>
    names()
  by_cols_w_compound <- by_cols
  if ("compound" %in% names(peaks)) {
    by_cols_w_compound <- c(by_cols, "compound")
  }

  # weak isotopocules and data groups
  has_weak_col <- "is_weak_isotopocule" %in% names(peaks)
  has_data_groups <- "data_group" %in% names(peaks)

  # calculate coverage
  isotopocule_coverage <-
    dataset |>
    orbi_get_isotopocule_coverage() |>
    dplyr::mutate(
      .by = dplyr::any_of(c("uidx", "filename")),
      y = as.integer(.data$isotopocule),
      xmin = if (x_column == "time.min") {
        .data$start_time.min -
          0.5 *
            (max(.data$end_time.min) - min(.data$start_time.min)) /
            (max(.data$end_scan.no) - min(.data$start_scan.no))
      } else {
        .data$start_scan.no - 0.5
      },
      xmax = if (x_column == "time.min") {
        .data$end_time.min +
          0.5 *
            (max(.data$end_time.min) - min(.data$start_time.min)) /
            (max(.data$end_scan.no) - min(.data$start_scan.no))
      } else {
        .data$end_scan.no + 0.5
      },
    )
  if (!"data_group" %in% names(isotopocule_coverage)) {
    isotopocule_coverage$data_group <- NA_integer_
  }

  # outlines (to show which isotopocules are recorded at all)
  scan_outlines <-
    peaks |>
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
      .by = by_cols
    ) |>
    dplyr::select(
      dplyr::all_of(by_cols_w_compound),
      "isotopocule",
      "xmin",
      "xmax"
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(y = as.integer(.data$isotopocule))

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
        .by = dplyr::all_of(c(by_cols_w_compound, "y", "data_group"))
      ) |>
      dplyr::group_by(!!!purrr::map(by_cols_w_compound, sym)) |>
      tidyr::complete(.data$y, .data$data_group) |>
      dplyr::ungroup() |>
      dplyr::left_join(
        peaks |> orbi_get_blocks_info() |> suppressMessages(),
        by = c(by_cols, "data_group")
      ) |>
      dplyr::mutate(
        .by = dplyr::any_of(c("uidx", "filename")),
        is_weak_isotopocule = ifelse(
          !is.na(.data$xmin),
          .data$is_weak_isotopocule,
          TRUE
        ),
        xmin = if (x_column == "time.min") {
          .data$start_time.min -
            0.5 *
              (max(.data$end_time.min) - min(.data$start_time.min)) /
              (max(.data$end_scan.no) - min(.data$start_scan.no))
        } else {
          .data$start_scan.no - 0.5
        },
        xmax = if (x_column == "time.min") {
          .data$end_time.min +
            0.5 *
              (max(.data$end_time.min) - min(.data$start_time.min)) /
              (max(.data$end_scan.no) - min(.data$start_scan.no))
        } else {
          .data$end_scan.no + 0.5
        }
      )
  }

  # make plot
  plot <-
    peaks |>
    ggplot2::ggplot() +
    ggplot2::aes(
      y = .data$y,
      xmin = .data$xmin,
      xmax = .data$xmax,
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
        map = ggplot2::aes(
          fill = "was flagged as weak",
          ymin = .data$y - 0.5,
          ymax = .data$y + 0.5
        ),
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
  if (add_data_blocks && has_blocks(peaks)) {
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
