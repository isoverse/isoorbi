# filter_isotopocules

test_that("filter_isotopocules() tests", {
  # failure
  expect_error(
    filter_isotopocules(),
    "argument \"dataset\" is missing, with no default",
    fixed = TRUE
  )

  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
    orbi_simplify_isox() |>
    suppressMessages()

  expect_error(
    filter_isotopocules(df),
    "argument \"isotopocules\" is missing, with no default",
    fixed = TRUE
  )

  suppressWarnings(expect_error(
    filter_isotopocules(df, isotopocules = "M0"),
    "none of the provided `isotopocules` are in the dataset",
    fixed = TRUE
  ))

  # success
  expect_true(is.tbl(filter_isotopocules(df, isotopocules = "17O")))
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
    "need a `dataset` data frame",
    fixed = TRUE
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

  expect_true(is.tbl(orbi_get_isotopocule_coverage(dataset)))
})

# orbi_plot_satellite_peaks

test_that("orbi_plot_satellite_peaks() tests", {
  # failure
  expect_error(
    orbi_plot_satellite_peaks(),
    "need a `dataset` data frame",
    fixed = TRUE
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
    "`dataset` requires column `is_satellite_peak` - make sure to run `orbi_flag_satellite_peaks()` first",
    fixed = TRUE
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
  expect_error(
    orbi_plot_raw_data(),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
    orbi_simplify_isox() |>
    orbi_flag_outliers(agc_fold_cutoff = 2) |>
    orbi_define_basepeak("M0") |>
    suppressMessages()

  expect_error(
    orbi_plot_raw_data(df),
    "`y` has to be provided, can be any expression valid in the data frame, common examples include intensity, ratio, tic * it.ms",
    fixed = TRUE
  )

  # success

  vdiffr::expect_doppelganger(
    "raw data ratio plot",
    orbi_plot_raw_data(df, y = ratio)
  )

  df2 <- orbi_read_isox(system.file(
    "extdata",
    "testfile_flow.isox",
    package = "isoorbi"
  )) |>
    orbi_simplify_isox() |>
    orbi_flag_outliers(agc_fold_cutoff = 2) |>
    suppressMessages()

  fig2 <- orbi_plot_raw_data(df2, y = "ratio", show_outliers = TRUE)

  expect_equal(length(fig2$layers), 1)
})

# orbi_plot_isotopocule_coverage

test_that("orbi_plot_isotopocule_coverage() tests", {
  # failure
  expect_error(
    orbi_plot_isotopocule_coverage(),
    "need a `dataset` data frame",
    fixed = TRUE
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
    "`isotopocules` has to be a character vector if provided",
    fixed = TRUE
  )

  # success

  suppressPackageStartupMessages(library(ggplot2))
  fig <- orbi_plot_isotopocule_coverage(df)

  expect_type(fig, "list")

  expect_equal(fig$facet$vars(), character(0))
})
