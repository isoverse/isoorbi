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
