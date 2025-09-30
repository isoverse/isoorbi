# orbi_filter_isotopocules

test_that("orbi_filter_isotopocules() tests", {
  # failure
  expect_error(
    orbi_filter_isotopocules(),
    "must be.*aggregated.*or.*data frame"
  )

  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
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

  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
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

  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
    orbi_simplify_isox() |>
    suppressMessages()

  dataset <- df |> factorize_dataset(c("filename", "compound", "isotopocule"))

  expect_snapshot_value(orbi_get_isotopocule_coverage(dataset), style = "json2")
})

# orbi_plot_satellite_peaks

test_that("orbi_plot_satellite_peaks() tests", {
  # failure
  expect_error(
    orbi_plot_satellite_peaks(),
    "must be.*aggregated.*or.*data frame"
  )

  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
    orbi_simplify_isox() |>
    suppressMessages()

  expect_error(
    orbi_plot_satellite_peaks(df),
    "dataset.* requires column.*is_satellite_peak.*orbi_flag_satellite_peaks()"
  )

  # success
  df2 <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
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

  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
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

  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
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
    "spectra no peak labels, no file name, no backgrounds",
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
