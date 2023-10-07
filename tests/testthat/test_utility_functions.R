# Tests: Common utility functions to clean and annotate data ------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("utility functions")

# orbi_flag_satellite_peaks
test_that("orbi_flag_satellite_peaks() tests", {

  # success
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(orbi_flag_satellite_peaks(orbi_simplify_isox(df))))

  # failure
  expect_error(orbi_flag_satellite_peaks(),
               "need a `dataset` data frame")

  expect_error(orbi_flag_satellite_peaks(dataset = T),
               "need a `dataset` data frame")

  expect_error(orbi_flag_satellite_peaks(dataset = df[, 1:5]),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`",
    fixed = TRUE)

  expect_error(orbi_flag_satellite_peaks(dataset = df[0,]),
               "something went wrong tying to flag satellite peaks: \nCaused by warning:\n! There was 1 warning in `dplyr::mutate()`.\nℹ In argument: `is_satellite_peak = .data$ions.incremental <\n  max(.data$ions.incremental)`.\nCaused by warning in `max()`:\n! no non-missing arguments to max; returning -Inf",
               fixed = TRUE)

  df2 <- df |> mutate(dummy = 1) |> select(-scan.no)

  expect_error(orbi_flag_satellite_peaks(dataset = df2),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`",
    fixed = TRUE)

})

# orbi_flag_weak_isotopocules
test_that("orbi_flag_weak_isotopocules() tests", {

  # failure
  expect_error(orbi_flag_weak_isotopocules(),
               "need a `dataset` data frame",
               fixed = TRUE)

  expect_error(orbi_flag_weak_isotopocules(dataset = T),
               "need a `dataset` data frame",
               fixed = TRUE)

  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_error(orbi_flag_weak_isotopocules(dataset = df),
    "`min_percent` needs to be a single number",
    fixed = TRUE)

  expect_error(orbi_flag_weak_isotopocules(dataset = df[, 1:5]),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`",
    fixed = TRUE)

  expect_error(orbi_flag_weak_isotopocules(dataset = df[0, ]),
               "`min_percent` needs to be a single number",
               fixed = TRUE)

  expect_error(orbi_flag_weak_isotopocules(dataset = df, min_percent = T),
    "`min_percent` needs to be a single number",
    fixed = TRUE)

  expect_error(orbi_flag_weak_isotopocules(dataset = df, min_percent = 100),
    "`min_percent` needs to be between 0 and 90",
    fixed = TRUE)

  expect_true(is.tbl(orbi_flag_weak_isotopocules(dataset = orbi_simplify_isox(df),
                                                   min_percent = 1)))

  df2 <-read.csv(file.path(base_dir, "test_files", "first10rows.csv")) |> select(-scan.no)

  expect_error(orbi_flag_weak_isotopocules(dataset = df2, min_percent = 1))

  df3 <- df |> mutate(
    block = as.factor("block1"),
    segment = as.factor("segment2"),
    injection = as.factor("injection3")
  )

  # success
  expect_true(is.tbl(orbi_flag_weak_isotopocules(dataset = df3, min_percent = 1)))

})

# orbi_flag_outliers

test_that("orbi_flag_outliers() tests", {

  # failure

  expect_error(orbi_flag_outliers(), "need a `dataset` data frame")

  expect_error(orbi_flag_outliers(dataset = T),
               "need a `dataset` data frame",
               fixed = TRUE)

  df <- read.csv(file.path(base_dir, "test_files", "first10rows.csv"), stringsAsFactors = T)


  expect_error(orbi_flag_outliers(dataset = df),
               "`intensity_window` needs to be a vector of two numbers (low and high filter) between 0 and 100",
               fixed = TRUE)

  expect_error(orbi_flag_outliers(dataset = df, intensity_window = T),
    "`intensity_window` needs to be a vector of two numbers (low and high filter) between 0 and 100",
    fixed = TRUE)

  expect_error(orbi_flag_outliers(dataset = df, intensity_window = 1000),
    "`intensity_window` needs to be a vector of two numbers (low and high filter) between 0 and 100",
    fixed = TRUE)

  expect_error(orbi_flag_outliers(dataset = df[, 1:5]),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `tic` and `it.ms`",
    fixed = TRUE)

  expect_error(orbi_flag_outliers(dataset = df[0,]),
               "`intensity_window` needs to be a vector of two numbers (low and high filter) between 0 and 100",
               fixed = TRUE)

  df2 <- df |> mutate(dummy=1) |> select(-scan.no)
  expect_error(orbi_flag_outliers(dataset = df2, intensity_window = 1),
               "`dataset` requires columns `filename`, `compound`, `scan.no`, `tic` and `it.ms`",
               fixed = TRUE)

  # success
  expect_true(is.tbl(orbi_flag_outliers(dataset = df, intensity_window = c(10,90))))

  df3 <-
    orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")) |> mutate(
      block = as.factor("block1"),
      segment = as.factor("segment2"),
      injection = as.factor("injection3")
    )

  expect_true(is.tbl(orbi_flag_outliers(dataset = df3, intensity_window = c(10,90))))

})

# orbi_define_basepeak()

test_that("orbi_define_basepeak() tests", {

  # failure

  expect_error(orbi_define_basepeak(), "need a `dataset` data frame",
               fixed = TRUE)

  expect_error(orbi_define_basepeak(dataset = T),
               "need a `dataset` data frame",
               fixed = TRUE)

  df <- read.csv(file.path(base_dir, "test_files", "first10rows.csv"), stringsAsFactors = T)

  expect_error(orbi_define_basepeak(dataset = df),
               "`basepeak_def` needs to be a single text value identifying the isotopocule to use as the basepeak", fixed = TRUE)

  expect_error(orbi_define_basepeak(dataset = df, basepeak_def = F),
    "`basepeak_def` needs to be a single text value identifying the isotopocule to use as the basepeak", fixed = TRUE)

  expect_error(orbi_define_basepeak(dataset = df,
                                    basepeak_def = c("M0", "123")),
    "`basepeak_def` needs to be a single text value identifying the isotopocule to use as the basepeak", fixed = TRUE)

  expect_error(orbi_define_basepeak(dataset = df,
                         basepeak_def = c("ABC123")),
    "`basepeak_def` is not an isotopocule in the dataset", fixed = TRUE)


  df2 <- df |> select(-scan.no)
  expect_error(orbi_define_basepeak(dataset = df2,
                                    basepeak_def = "M0"),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `isotopocule`, and `ions.incremental`", fixed = TRUE)


  # success
  expect_true(is.tbl(orbi_define_basepeak(
    dataset = df, basepeak_def = "M0"
    )))

})
