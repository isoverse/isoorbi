# orbi_filter_isotopocules

test_that("orbi_filter_isotopocules() tests", {
  # failure
  expect_error(
    orbi_filter_isotopocules(),
    "must be.*aggregated.*or.*data frame"
  )

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    suppressMessages()

  expect_message(
    orbi_filter_isotopocules(df),
    "kept all"
  )

  suppressWarnings(expect_error(
    orbi_filter_isotopocules(df, isotopocules = "M0"),
    "none of the provided.*isotopocules.*are in the dataset"
  ))

  # success
  expect_message(
    orbi_filter_isotopocules(df, isotopocules = "17O"),
    "remove.*because.*were.*not.*isotopocule.*17O"
  )
})

# dynamic_y_scale

test_that("dynamic_y_scale() tests", {
  # failure
  expect_error(
    dynamic_y_scale(),
    "argument \"plot\" is missing, with no default",
    fixed = TRUE
  )

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    orbi_define_basepeak(basepeak_def = "15N") |>
    orbi_analyze_shot_noise() |>
    suppressMessages()
})

# orbi_get_isotopocule_coverage

test_that("orbi_get_isotopocule_coverage() tests", {
  # failure
  expect_error(
    orbi_get_isotopocule_coverage(),
    "must be.*aggregated.*or.*data frame"
  )

  # success

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    suppressMessages()

  dataset <- df |> factorize_dataset(c("filename", "compound", "isotopocule"))

  expect_snapshot_value(orbi_get_isotopocule_coverage(dataset), style = "json2")

  # peak flags are used as an additional grouping if they are available
  raw <- orbi_read_raw(
    orbi_get_example_files("nitrate_test_10scans.raw"),
    show_progress = FALSE,
    show_problems = FALSE
  ) |>
    orbi_aggregate_raw(show_progress = FALSE, show_problems = FALSE) |>
    orbi_identify_isotopocules(
      c("M0" = 61.9878, "15N" = 62.9850, "17O" = 62.9922, "18O" = 63.9922)
    ) |>
    suppressMessages()

  raw_coverage <- orbi_get_isotopocule_coverage(raw) |> suppressMessages()
  expect_true("centroiderFlags" %in% names(raw_coverage))
  # M0 has both unflagged and exception peaks, so it must appear in both groups
  expect_setequal(
    raw_coverage |>
      dplyr::filter(.data$isotopocule == "M0") |>
      dplyr::pull(.data$centroiderFlags) |>
      as.character() |>
      unique(),
    c("none", "exception")
  )
  # without the flags the stretches would be merged, with them there are more
  expect_gt(
    nrow(raw_coverage),
    raw_coverage |>
      dplyr::select(-"centroiderFlags") |>
      dplyr::distinct() |>
      nrow() -
      1L
  )
})

# orbi_plot_satellite_peaks

test_that("orbi_plot_satellite_peaks() tests", {
  # failure
  expect_error(
    orbi_plot_satellite_peaks(),
    "must be.*aggregated.*or.*data frame"
  )

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    suppressMessages()

  expect_error(
    orbi_plot_satellite_peaks(df),
    "dataset.* requires column.*is_satellite_peak.*orbi_flag_satellite_peaks()"
  )

  # success
  df2 <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    orbi_flag_satellite_peaks() |>
    suppressMessages()

  vdiffr::expect_doppelganger(
    "satellite peaks plots",
    orbi_plot_satellite_peaks(df2)
  )
})

# orbi_plot_raw_data

test_that("orbi_plot_raw_data() tests", {
  # failure
  expect_error(orbi_plot_raw_data(), "must be.*aggregated.*or.*data frame")

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    orbi_flag_outliers(agc_fold_cutoff = 2) |>
    suppressMessages()

  expect_error(orbi_plot_raw_data(df), "y.*can be any expression valid")
  expect_error(
    orbi_plot_raw_data(df, y = ions.incremental, x = "dne"),
    "must.*time.min.*scan.no"
  )
  expect_error(
    orbi_plot_raw_data(df, y = ions.incremental, y_scale = "dne"),
    "must.*raw.*linear.*pseudo-log.*log"
  )

  vdiffr::expect_doppelganger(
    "raw data ions plot",
    orbi_plot_raw_data(df, y = ions.incremental, y_scale = "log")
  )

  # data points in addition to the lines
  geoms <- function(plot) {
    unname(purrr::map_chr(plot$layers, ~ class(.x$geom)[1]))
  }
  expect_false(
    "GeomPoint" %in%
      geoms(orbi_plot_raw_data(df, y = ions.incremental, show_outliers = FALSE))
  )
  expect_equal(
    geoms(
      orbi_plot_raw_data(
        df,
        y = ions.incremental,
        show_outliers = FALSE,
        show_points = TRUE
      )
    ),
    c("GeomLine", "GeomPoint")
  )
  expect_error(
    orbi_plot_raw_data(df, y = ions.incremental, show_points = "yes"),
    "show_points.*must be TRUE or FALSE"
  )
  # groups with a single data point are not part of the lines but they are
  # still in the points
  df_singles <- df |> dplyr::mutate(data_group = scan.no)
  singles_plot <- orbi_plot_raw_data(
    df_singles,
    y = ions.incremental,
    show_points = TRUE
  ) |>
    ggplot2::ggplot_build()
  expect_equal(nrow(singles_plot$data[[1]]), 0L)
  expect_equal(nrow(singles_plot$data[[2]]), nrow(df_singles))
  df_some_singles <- df |>
    dplyr::mutate(data_group = ifelse(scan.no %% 50 == 0, -scan.no, 1L))
  expect_equal(
    orbi_plot_raw_data(df_some_singles, y = ions.incremental) |>
      ggplot2::layer_data(1) |>
      nrow(),
    sum(df_some_singles$scan.no %% 50 != 0)
  )

  # point size: ggplot default unless set, then for data AND outlier points
  df_outliers <- df |>
    dplyr::mutate(
      is_outlier = scan.no %% 100 == 0,
      outlier_type = ifelse(is_outlier, "test", NA_character_)
    )
  expect_true(any(df_outliers$is_outlier))
  point_sizes <- function(plot) {
    plot$layers[geoms(plot) == "GeomPoint"] |>
      purrr::map(~ .x$aes_params$size) |>
      unname()
  }
  expect_equal(
    point_sizes(
      orbi_plot_raw_data(df_outliers, y = ions.incremental, show_points = TRUE)
    ),
    list(NULL, NULL)
  )
  expect_equal(
    point_sizes(
      orbi_plot_raw_data(
        df_outliers,
        y = ions.incremental,
        show_points = TRUE,
        point_size = 2L
      )
    ),
    list(2L, 2L)
  )
  # also for the outliers if the data points are not shown
  expect_equal(
    point_sizes(
      orbi_plot_raw_data(df_outliers, y = ions.incremental, point_size = 2)
    ),
    list(2)
  )
  expect_error(
    orbi_plot_raw_data(df, y = ions.incremental, point_size = 0),
    "point_size.*must be a single number larger than 0"
  )

  # time axis labels (durations, optionally short) vs. scan axis labels
  # note: breaks outside the plotted range have NA labels
  x_labels <- function(plot) {
    labels <- ggplot2::ggplot_build(plot)$layout$panel_params[[
      1
    ]]$x$get_labels()
    labels[!is.na(labels)]
  }
  expect_equal(
    x_labels(orbi_plot_raw_data(df, y = ions.incremental)),
    c("0:30 min", "1:00 min", "1:30 min", "2:00 min", "2:30 min", "3:00 min")
  )
  expect_equal(
    x_labels(
      orbi_plot_raw_data(df, y = ions.incremental, short_time_labels = TRUE)
    ),
    c("0:30m", "1:00m", "1:30m", "2:00m", "2:30m", "3:00m")
  )
  # no effect on scan based axes
  expect_equal(
    x_labels(
      orbi_plot_raw_data(
        df,
        y = ions.incremental,
        x = "scan.no",
        short_time_labels = TRUE
      )
    ),
    x_labels(orbi_plot_raw_data(df, y = ions.incremental, x = "scan.no"))
  )
  expect_error(
    orbi_plot_raw_data(df, y = ions.incremental, short_time_labels = "yes"),
    "short_time_labels.*must be TRUE or FALSE"
  )

  # number of pretty breaks, on both time and scan axes
  expect_gt(
    length(x_labels(orbi_plot_raw_data(
      df,
      y = ions.incremental,
      n_x_breaks = 10
    ))),
    length(x_labels(orbi_plot_raw_data(df, y = ions.incremental)))
  )
  expect_lt(
    length(x_labels(
      orbi_plot_raw_data(
        df,
        y = ions.incremental,
        x = "scan.no",
        n_x_breaks = 2
      )
    )),
    length(x_labels(orbi_plot_raw_data(
      df,
      y = ions.incremental,
      x = "scan.no"
    )))
  )
  # but not together with specific breaks
  expect_error(
    orbi_plot_raw_data(
      df,
      y = ions.incremental,
      x_breaks = 1:2,
      n_x_breaks = 3
    ),
    "either.*x_breaks.*or.*n_x_breaks.*not both"
  )
  expect_no_error(
    orbi_plot_raw_data(df, y = ions.incremental, x_breaks = c(1, 2))
  )
  expect_error(
    orbi_plot_raw_data(df, y = ions.incremental, n_x_breaks = 0),
    "n_x_breaks.*must be a single whole number"
  )
  expect_error(
    orbi_plot_raw_data(df, y = ions.incremental, n_x_breaks = 2.5),
    "n_x_breaks.*must be a single whole number"
  )

  df2 <- orbi_read_isox(system.file(
    "extdata",
    "testfile_flow.isox",
    package = "isoorbi"
  )) |>
    orbi_simplify_isox() |>
    orbi_flag_outliers(agc_fold_cutoff = 2) |>
    orbi_define_basepeak("M0") |>
    suppressMessages()

  expect_error(
    orbi_plot_raw_data(df2, y = ratio, isotopocules = "dne"),
    "none.*are in the dataset"
  )

  vdiffr::expect_doppelganger(
    "raw data ratio plot",
    orbi_plot_raw_data(df2, y = ratio, x = "scan.no", show_outliers = TRUE)
  )
})

# orbi_plot_isotopocule_coverage

test_that("orbi_plot_isotopocule_coverage() tests", {
  # failure
  expect_error(
    orbi_plot_isotopocule_coverage(),
    "must be.*aggregated.*or.*data frame"
  )

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    suppressMessages()

  expect_error(
    orbi_plot_isotopocule_coverage(df, isotopocules = 42),
    "must be a character vector"
  )

  # success

  vdiffr::expect_doppelganger(
    "coverage plot",
    orbi_plot_isotopocule_coverage(df)
  )

  # with peak flags available the detected isotopocules are split by their flags
  raw <- orbi_read_raw(
    orbi_get_example_files("nitrate_test_10scans.raw"),
    show_progress = FALSE,
    show_problems = FALSE
  ) |>
    orbi_aggregate_raw(show_progress = FALSE, show_problems = FALSE) |>
    orbi_identify_isotopocules(
      c("M0" = 61.9878, "15N" = 62.9850, "17O" = 62.9922, "18O" = 63.9922)
    ) |>
    suppressMessages()

  raw_plot <- orbi_plot_isotopocule_coverage(raw) |> suppressMessages()
  fill_scale <- raw_plot$scales$scales[[
    which(purrr::map_lgl(raw_plot$scales$scales, ~ "fill" %in% .x$aesthetics))
  ]]
  expect_equal(
    fill_scale$breaks,
    c("isotopocules (no flags)", "isotopocules (exception)", "not detected")
  )
  # unflagged stays black, flagged picks up the first color, missing stays white
  expect_equal(
    fill_scale$palette(3),
    c(
      "isotopocules (no flags)" = "black",
      "isotopocules (exception)" = "#7570B3",
      "not detected" = "white"
    )
  )

  vdiffr::expect_doppelganger(
    "coverage plot with flags",
    raw_plot
  )
})

test_that("orbi_plot_spectra()", {
  # test file
  test_file <- system.file(
    "extdata",
    "nitrate_test_10scans.raw",
    package = "isoorbi"
  ) |>
    orbi_read_raw(include_spectra = c(1, 10)) |>
    orbi_aggregate_raw(aggregator = "minimal") |>
    suppressMessages()

  # errors
  orbi_plot_spectra() |> expect_error("must be.*aggregated")
  orbi_plot_spectra(test_file, mz_min = "test") |>
    expect_error("must be a single number")
  orbi_plot_spectra(test_file, mz_max = "test") |>
    expect_error("must be a single number")
  orbi_plot_spectra(test_file, mz_base_peak = "test") |>
    expect_error("must be a single number")
  orbi_plot_spectra(test_file, mz_base_peak = "test") |>
    expect_error("must be a single number")
  orbi_plot_spectra(test_file, mz_focus_nominal_offsets = integer()) |>
    expect_error("vector of positive integers")
  orbi_plot_spectra(test_file, max_scans = -1) |>
    expect_error("positive integer")
  orbi_plot_spectra(test_file, max_files = 0) |>
    expect_error("positive integer")
  orbi_plot_spectra(test_file, label_peaks = 42) |>
    expect_error("TRUE or FALSE")
  orbi_plot_spectra(test_file, show_filenames = 1) |>
    expect_error("TRUE or FALSE")
  orbi_plot_spectra(test_file, show_focus_backgrounds = 0) |>
    expect_error("TRUE or FALSE")
  orbi_plot_spectra(test_file, background_colors = c("blue")) |>
    expect_error("character vector")

  # success
  vdiffr::expect_doppelganger("spectra 2 scans", orbi_plot_spectra(test_file))
  vdiffr::expect_doppelganger(
    "spectra 1 scan",
    orbi_plot_spectra(test_file, max_scans = 1) |> suppressMessages()
  )
  vdiffr::expect_doppelganger(
    "spectra - defaults off",
    orbi_plot_spectra(
      test_file,
      show_focus_backgrounds = FALSE,
      show_filenames = FALSE,
      label_peaks = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    "spectra - single spectrum only",
    orbi_plot_spectra(test_file, mz_min = 62.5, mz_max = 63.5)
  )
  vdiffr::expect_doppelganger(
    "spectra - base peak spectrum only",
    orbi_plot_spectra(test_file, mz_focus_nominal_offsets = 0)
  )

  # get isotopcules
  isotopocules <- tibble(
    compound = "nitrate",
    isotopocule = c("M0", "15N", "17O", "18O"),
    mass = c(61.9878, 62.9850, 62.9922, 63.9922),
    tolerance = 1,
    charge = 1
  )
  test_file <- test_file |>
    orbi_identify_isotopocules(isotopocules) |>
    suppressMessages()

  vdiffr::expect_doppelganger(
    "spectra - M+1 and M+2 only",
    orbi_plot_spectra(test_file, mz_focus_nominal_offsets = c(1, 2))
  )

  vdiffr::expect_doppelganger(
    "spectra - all but without unknown peaks",
    test_file |>
      orbi_filter_isotopocules() |>
      suppressMessages() |>
      orbi_plot_spectra()
  )
})
