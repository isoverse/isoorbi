#' Analyze shot noise
#'
#' will calculate for all combinations of `filename`, `compound`, and `isotopocule` in the provided `dataset`
#'
#' @title Shot noise calculation
#' @description This function computes the shot noise calculation.
#' @inheritParams orbi_flag_satellite_peaks
#' @param include_flagged_data whether to include flagged data in the shot noise calculation (FALSE by default)
#' @return The processed data frame with new columns: `n_effective_ions`, `ratio`, `ratio_rel_se.permil`, `shot_noise.permil`
#' @export
orbi_analyze_shot_noise <- function(dataset, include_flagged_data = FALSE) {
  # safety checks
  check_dataset_arg(dataset)
  dataset <- if (is(dataset, "orbi_aggregated_data")) {
    # make sure we don't interpret unidentified and missing peaks
    dataset <- orbi_filter_isotopocules(dataset) |> suppressMessages()
    dataset$file_info |>
      dplyr::select("uidx", "filename") |>
      right_join(
        dataset$scans |>
          dplyr::select(
            "uidx",
            "scan.no",
            "time.min",
            dplyr::any_of("is_outlier")
          ),
        by = "uidx"
      ) |>
      right_join(dataset$peaks, by = c("uidx", "scan.no"))
  } else {
    dataset
  }
  check_arg(
    dataset,
    "basepeak_ions" %in% names(dataset),
    "requires defined basepeak (column {.field basepeak_ions}), make sure to run {.strong orbi_define_basepeak()} first"
  )
  check_tibble(
    dataset,
    req_cols = c(
      "uidx|filename",
      "isotopocule",
      "ions.incremental",
      "basepeak_ions",
      "ratio"
    ),
    regexps = TRUE
  )

  # filter flagged data
  n_all <- nrow(dataset)
  dataset_wo_flagged <- dataset |> filter_flagged_data()
  n_after <- nrow(dataset_wo_flagged)
  if (!include_flagged_data) {
    dataset <- dataset_wo_flagged
  }

  # info message
  start <- start_info("is running")
  order_cols <- tidyselect::eval_select(
    any_of(c(
      "uidx",
      "filename",
      "itc_uidx",
      "compound",
      "fragment",
      "isotopocule",
      "scan.no"
    )),
    dataset
  ) |>
    names()

  # calculation
  out <-
    dataset |>
    # preserve original row order
    dplyr::mutate(..idx = dplyr::row_number()) |>
    # make sure order is compatible with cumsum based calculations
    dplyr::arrange(!!!purrr::map(order_cols, sym)) |>
    dplyr::mutate(
      # group by filename, compound, isotopocule.
      .by = dplyr::any_of(c(
        "uidx",
        "filename",
        "itc_uidx",
        "compound",
        "fragment",
        "isotopocule"
      )),
      # cumulative ions
      n_isotopocule_ions = cumsum(.data$ions.incremental),
      n_basepeak_ions = cumsum(.data$basepeak_ions),
      n_effective_ions = .data$n_basepeak_ions *
        .data$n_isotopocule_ions /
        (.data$n_basepeak_ions + .data$n_isotopocule_ions),
      # relative standard error
      n_measurements = 1:n(),
      ratio_mean = cumsum(.data$ratio) / .data$n_measurements,
      ratio_gmean = exp(cumsum(log(.data$ratio)) / .data$n_measurements),
      #ratio_sd = sqrt( cumsum( (ratio - ratio_mean)^2 ) / (n_measurements - 1L) ),
      # alternative std. dev. formula compatible with cumsum
      # note that adjustment for sample sdev --> sqrt(n/(n-1)) partially cancels with 1/sqrt(n) for SD to SE
      ratio_se = sqrt(
        cumsum(.data$ratio^2) / .data$n_measurements - .data$ratio_mean^2
      ) *
        sqrt(1 / (.data$n_measurements - 1L)),
      # Q: do you really want to use gmean here but not in the std. dev calculation?
      ratio_rel_se.permil = 1000 * .data$ratio_se / .data$ratio_gmean,
      # shot noise - calc is same as 1000 * (1 / n_basepeak_ions + 1 / n_isotopocule_ions) ^ 0.5 but faster
      shot_noise.permil = 1000 * .data$n_effective_ions^-0.5
    ) |>
    # restor original row order
    dplyr::arrange(.data$..idx) |>
    # remove columns not usually used downstream
    select(
      -"..idx",
      -"n_basepeak_ions",
      -"n_isotopocule_ions",
      -"n_measurements",
      -"ratio_mean",
      -"ratio_gmean",
      -"ratio_se"
    ) |>
    try_catch_cnds()

  # stop if error
  abort_cnds(out$conditions)
  dataset <- out$result

  # info
  finish_info(
    "analyzed the shot noise for {if (include_flagged_data) format_number(n_all) else format_number(n_after)} {.field ratios} (",
    if (n_all == n_after) {
      "there are no flagged peaks)"
    } else {
      "{if (include_flagged_data) 'including' else 'excluding'} {format_number(n_all - n_after)} flagged peaks)"
    },
    start = start
  )

  # return
  return(dataset)
}

#' plot shot noise
#' @title Make a shot noise plot
#' @description This function creates a shot noise plot using a `shotnoise` data frame created by the [orbi_analyze_shot_noise()] function.
#' @param shotnoise a `shotnoise` data frame
#' @param x x-axis for the shot noise plot, either "time.min" or "n_effective_ions"
#' @param color which column to use for the color aesthetic (must be a factor)
#' @param colors which colors to use, by default a color-blind friendly color palettes (RColorBrewer, dark2)
#' @param permil_target highlight the target permil in the shotnoise plot
#' @return a ggplot object
#' @export
orbi_plot_shot_noise <- function(
  shotnoise,
  x = c("time.min", "n_effective_ions"),
  permil_target = NA_real_,
  color = "ratio_label",
  colors = c(
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666"
  )
) {
  # safety checks
  ## shotnoise
  check_tibble(
    shotnoise,
    c("filename", "compound", "compound", "isotopocule", "basepeak")
  )
  check_arg(
    shotnoise,
    all(c("ratio_rel_se.permil", "shot_noise.permil") %in% names(shotnoise)),
    format_inline(
      "requires columns {.field ratio_rel_se.permil} and {.field shot_noise.permil}, make sure to run {.fun orbi_analyze_shot_noise} first"
    ),
    include_type = FALSE
  )
  # x_column
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
    "`shotnoise` requires a factor column set for `color` aesthetic" = is_scalar_character(
      color
    ) &&
      color %in% names(plot_df) &&
      is.factor(plot_df[[color]])
  )
  if (length(levels(plot_df[[color]])) > length(colors)) {
    sprintf(
      "not enough `colors` provided, %d needed for distinguishing %s",
      length(levels(plot_df[[color]])),
      color
    ) |>
      abort()
  }

  # closest analysis marker
  find_closest_analysis <- function(target.permil) {
    function(df) {
      df |>
        dplyr::group_by(.data$filename, .data$compound, .data$ratio_label) |>
        dplyr::mutate(
          target.permil = !!target.permil,
          diff_target.permil = abs(.data$ratio_rel_se.permil - target.permil)
        ) |>
        dplyr::arrange(.data$diff_target.permil) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup()
    }
  }

  # standard plot
  plot <-
    plot_df |>
    ggplot2::ggplot() +
    ggplot2::aes(
      y = .data$ratio_rel_se.permil,
      color = !!sym(color),
      shape = !!sym(color),
      group = paste(.data$filename, .data$compound, .data$isotopocule)
    ) +
    ggplot2::geom_line(
      map = ggplot2::aes(y = .data$shot_noise.permil, linetype = !!sym(color))
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_log10(breaks = 10^(-4:4), labels = function(x) {
      paste(x, "\U2030")
    }) +
    ggplot2::annotation_logticks(sides = "lb") +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_shape_manual(values = c(21:25, 15:18)) +
    ggplot2::scale_linetype_manual(values = rep(1, 8)) +
    orbi_default_theme() +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(
      y = "relative error",
      color = "data",
      shape = "data",
      linetype = "shot noise"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(linetype = 0)),
      linetype = ggplot2::guide_legend(
        override.aes = list(color = colors[1:length(levels(plot_df[[color]]))])
      )
    )

  # wrap
  plot <- plot |> dynamic_wrap()

  # plots vs. time
  if (x_column == "time.min") {
    plot <- plot +
      ggplot2::aes(x = .data$time.min) +
      ggplot2::scale_x_log10(breaks = 10^(-2:2), labels = paste) +
      ggplot2::labs(x = "analysis time [min]")
  } else if (x_column == "n_effective_ions") {
    plot <- plot +
      ggplot2::aes(x = .data$n_effective_ions) +
      ggplot2::scale_x_log10(breaks = 10^(2:12), labels = scales::label_log()) +
      ggplot2::labs(x = "counts")
  }

  # permil target
  if (!is.na(permil_target)) {
    plot <- plot +
      # closest analysis
      ggplot2::geom_hline(yintercept = permil_target, linetype = 2) +
      ggplot2::geom_vline(
        data = find_closest_analysis(target.permil = permil_target),
        # vs. time
        map = ggplot2::aes(xintercept = .data$time.min, color = NULL),
        linetype = 2
      )
  }

  # return
  return(plot)
}
