
#' Visualize satellite peaks
#' 
#' Call this function any time after flagging the satellite peaks to see where they are. Use the `isotopocules` argument to focus on the specific isotopocules of interest.
#' 
#' @param dataset isox dataset with satellite peaks identified (`orbi_flag_satellite_peaks()`)
#' @param isotopocules which isotopocules to visualize
#' @param x x-axis for the plot, either "time.min" (the default) or "scan.no"
#' @param colors which colors to use, by default a color-blind friendly color palettes (RColorBrewer, dark2)
#' @export
orbi_plot_satellite_peaks <- function(
    dataset, isotopocules, x = c("scan.no", "time.min"), 
    colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")) {
  
  # safety checks
  cols <- c("filename", "compound", "scan.no", "time.min", "isotopocule", "ions.incremental")
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "need at least one value for `isotopocules`" = !missing(isotopocules) && is_character(isotopocules) && length(isotopocules) > 0L,
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`" =
      all(cols %in% names(dataset)),
    "`dataset` requires column `is_satellite_peak` - make sure to run `orbi_flag_satellite_peaks()` first" = "is_satellite_peak" %in% names(dataset)
  )
  x_column <- arg_match(x)
  
  # check provided isotopocules
  dataset <- dataset |> factorize_dataset(c("filename", "compound", "isotopocule"))
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
  plot_df <- dataset |>
    dplyr::filter(.data$isotopocule %in% isotopocules) |>
    droplevels()
  
  # make plot
  plot <- plot_df |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = !!sym(x_column), y = .data$ions.incremental, 
      color = .data$isotopocule) +
    ggplot2::geom_line(data = function(df) dplyr::filter(df, !.data$is_satellite_peak)) + 
    ggplot2::geom_point(
      data = function(df) dplyr::filter(df, .data$is_satellite_peak) |> dplyr::mutate(flagged = "satellite peaks"), 
      map = aes(shape = .data$flagged),
      alpha = 0.5
    ) + 
    ggplot2::scale_y_log10(breaks = 10^(0:20), labels = scales::label_log()) +
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
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(shape = NA), order = 1)
    )
  
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