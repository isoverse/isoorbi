
#' Analyze shot noise
#'
#' will calculate for all combinations of `filename`, `compound`, and `isotopocule` in the provided `dataset`
#'
#' @title Shot noise calculation
#' @description This function computes the shot noise calculation
#' @param dataset a data frame output after running `orbi_define_basepeak()`
#' @return The processed data frame with new columns: `n_effective_ions`, `ratio`, `ratio_rel_se.permil`, `shot_noise.permil`
#' @export
orbi_analyze_shot_noise <- function(dataset){

  ions.incremental <- basepeak_ions <- n_basepeak_ions <- n_isotopocule_ions <- ratio <- n_measurements <- ratio_mean <- ratio_se <- ratio_gmean <- n_effective_ions <- ..idx <- NULL

  # safety checks
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filename`, `compound` and `isotopocule`" = all(c("filename", "compound", "isotopocule") %in% names(dataset)),
    "`dataset` requires defined basepeak (column `basepeak_ions`), make sure to run `orbi_define_basepeak()` first" = "basepeak_ions" %in% names(dataset)
  )

  # info message
  start_time <-
    sprintf(
      "orbi_analyze_shot_noise() is analyzing the shot noise for %d peaks... ",
      nrow(dataset)
    ) |> message_start()

  # calculation
  dataset <-
    try_catch_all(
      dataset |>
        # preserve original row order
        dplyr::mutate(..idx = dplyr::row_number()) |>
        # make sure order is compatible with cumsum based calculations
        dplyr::arrange(.data$filename, .data$compound, .data$isotopocule, .data$scan.no) |>
        dplyr::mutate(
          # group by filename, compound, isotopocule.
          .by = c("filename", "compound", "isotopocule"),
          # cumulative ions
          n_isotopocule_ions = cumsum(ions.incremental),
          n_basepeak_ions = cumsum(basepeak_ions),
          n_effective_ions = n_basepeak_ions * n_isotopocule_ions / (n_basepeak_ions + n_isotopocule_ions),
          # relative standard error
          n_measurements = 1:n(),
          ratio = .data$ions.incremental / .data$basepeak_ions,
          ratio_mean = cumsum(ratio) / n_measurements,
          ratio_gmean = exp(cumsum(log(ratio)) / n_measurements),
          #ratio_sd = sqrt( cumsum( (ratio - ratio_mean)^2 ) / (n_measurements - 1L) ),
          # alternative std. dev. formula compatible with cumsum
          # note that adjustment for sample sdev --> sqrt(n/(n-1)) partially cancels with 1/sqrt(n) for SD to SE
          ratio_se = sqrt(cumsum(ratio^2) / n_measurements - ratio_mean^2) * sqrt(1/(n_measurements - 1L)),
          # Q: do you really want to use gmean here but not in the std. dev calculation?
          ratio_rel_se.permil = 1000 * ratio_se / ratio_gmean,
          # shot noise - calc is same as 1000 * (1 / n_basepeak_ions + 1 / n_isotopocule_ions) ^ 0.5 but faster
          shot_noise.permil = 1000 * n_effective_ions ^ -0.5
        ) |>
        # restor original row order
        dplyr::arrange(..idx) |>
        # remove columns not usually used downstream
        select(-"..idx", -"n_basepeak_ions", -"n_isotopocule_ions", -"n_measurements", -"ratio_mean", -"ratio_gmean", -"ratio_se"),
      "something went wrong analyzing shot noise",
      newline = TRUE
    )

  # info
  sprintf("calculations finished") |> message_finish(start_time = start_time)

  # return
  return(dataset)
}

#' plot shot noise
#' @param shotnoise a `shotnoise` data frame
#' @param x x-axis for the shot noise plot, either "time.min" or "n_effective_ions"
#' @param color which column to use for the color aesthetic (must be a factor)
#' @param colors which colors to use, by default a color-blind friendly color palettes (RColorBrewer, dark2)
#' @param permil_target highlight the t
#' @export
orbi_plot_shot_noise <- function(
    shotnoise,
    x = c("time.min", "n_effective_ions"),
    permil_target = NA_real_,
    color = "ratio_label",
    colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")) {

  filename <- compound <- ratio_label <- ratio_rel_se.permil <- diff_target.permil <- ratio_rel_se.permil <- isotopocule <- shot_noise.permil <- element_blank <- time.min <- n_effective_ions <- NULL

  # safety checks
  stopifnot(
    "need a `shotnoise` data frame" = !missing(shotnoise) && is.data.frame(shotnoise),
    "`shotnoise` requires columns `filename`, `compound`, `isotopocule` and `basepeak`" =
      all(c("filename", "compound", "compound", "isotopocule", "basepeak") %in% names(shotnoise)),
    "`shotnoise` requires columns `ratio_rel_se.permil` and `shot_noise.permil`, make sure to run `orbi_analyze_shot_noise()` first" =
      all(c("ratio_rel_se.permil", "shot_noise.permil") %in% names(shotnoise))
  )
  x_column <- arg_match(x)

  # data
  plot_df <- shotnoise |>
    filter(!is.na(.data$ratio_rel_se.permil)) |>
    arrange(.data$isotopocule) |>
    mutate(
      ratio_label = sprintf("%s/%s", .data$isotopocule, .data$basepeak) |>
        factor_in_order()
    )

  # color checks
  stopifnot(
    "`shotnoise` requires a factor column set for `color` aesthetic" = is_scalar_character(color) && color %in% names(plot_df) && is.factor(plot_df[[color]])
  )
  if (length(levels(plot_df[[color]])) > length(colors))
    sprintf("not enough `colors` provided, %d needed for distinguishing %s", length(levels(plot_df[[color]])), color) |>
    abort()

  # closest analysis marker
  find_closest_analysis <- function(target.permil) {
    function(df) {
      df |>
        dplyr::group_by(filename, compound, ratio_label) |>
        dplyr::mutate(
          target.permil = !!target.permil,
          diff_target.permil = abs(ratio_rel_se.permil - target.permil)
        ) |>
        dplyr::arrange(diff_target.permil) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
    }
  }

  # standard plot
  rcolor_brewer_dark2 <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
  plot <-
    plot_df |>
    ggplot2::ggplot() +
    ggplot2::aes(y = ratio_rel_se.permil, color = !!sym(color), shape = !!sym(color), group = paste(filename, compound, isotopocule)) +
    ggplot2::geom_line(
      map = ggplot2::aes(y = shot_noise.permil, linetype = !!sym(color))
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_log10(breaks = 10^(-4:4), labels = function(x) paste(x, "\U2030")) +
    ggplot2::annotation_logticks(sides = "lb") +
    # use a color-blind friendly palette
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_shape_manual(values = c(21:25, 15:18)) +
    ggplot2::scale_linetype_manual(values = rep(1, 8)) +
    #expand_limits(y = c(0.1, 10)) +
    # FIXME: include filename only if there is more than one file
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      strip.text = ggplot2::element_text(size = 20),
      panel.grid = ggplot2::element_blank(),
      #panel.border = element_blank(), # MAYBE
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      #axis.line.y.left = ggplot2::element_line(), # MAYBE
      #axis.line.x.bottom = ggplot2::element_line(), # MAYBE
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title.align = 0.5,
      legend.background = element_blank()
    ) +
    ggplot2::labs(
      y = "relative error",
      color = "data", shape = "data",
      linetype = "shot noise"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(linetype = 0)),
      linetype = ggplot2::guide_legend(override.aes = list(color = colors[1:length(levels(plot_df[[color]]))]))
    )

  # wrap
  n_files <- length(levels(plot_df$filename))
  n_compounds <- length(levels(plot_df$compound))
  if (n_files > 1L && n_compounds > 1L) {
    plot <- plot + ggplot2::facet_wrap(~filename + compound)
  } else if (n_compounds > 1L) {
    plot <- plot + ggplot2::facet_wrap(~compound)
  } else if (n_files > 1L) {
    plot <- plot + ggplot2::facet_wrap(~filename)
  }

  # plots vs. time
  if (x_column == "time.min") {
    plot <- plot %+%
      ggplot2::aes(x = time.min) +
      ggplot2::scale_x_log10(breaks = 10^(-2:2), labels = paste) +
      ggplot2::labs(x = "analysis time [min]")
  } else if (x_column == "n_effective_ions") {
    plot <- plot %+%
      ggplot2::aes(x = n_effective_ions) +
      ggplot2::scale_x_log10(breaks = 10^(2:12), labels = scales::label_log()) +
      ggplot2::labs(x = "counts")
  }

  # permil target
  if (!is.na(permil_target)) {
    plot <- plot +
      # closest analysis
      ggplot2::geom_hline(yintercept = permil_target , linetype = 2) +
      ggplot2::geom_vline(
        data = find_closest_analysis(target.permil = permil_target),
        # vs. time
        map = ggplot2::aes(xintercept = time.min, color = NULL),
        linetype = 2
      )
  }

  # return
  return(plot)
}
