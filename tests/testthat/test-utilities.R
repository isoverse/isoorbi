# Tests: utility functions to clean and annotate data ------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

# Internal utility functions =============

# factorize_dataset
test_that("factorize_dataset() tests", {
  # failure
  expect_error(
    factorize_dataset(),
    "argument \"dataset\" is missing, with no default"
  )

  df <- suppressMessages(
    orbi_read_isox(system.file(
      "extdata",
      "testfile_dual_inlet.isox",
      package = "isoorbi"
    ))
  )

  expect_silent(factorize_dataset(df))
})

# group_if_exists
test_that("group_if_exists() tests", {
  # failure
  expect_error(
    group_if_exists(),
    "argument \"cols\" is missing, with no default"
  )

  expect_error(
    group_if_exists(42),
    "argument \"cols\" is missing, with no default"
  )
})

# group_by_same_groups
test_that("group_by_same_groups() tests", {
  # failure
  expect_error(
    group_by_same_groups(),
    "argument \"target_dataset\" is missing, with no default"
  )

  expect_error(
    group_by_same_groups(42),
    "no applicable method for 'group_by' applied to an object of class \"c('double', 'numeric')\"",
    fixed = TRUE
  )
})

# count_grouped_distinct
test_that("count_grouped_distinct() tests", {
  # failure
  expect_error(
    count_grouped_distinct(),
    "argument \"dataset\" is missing, with no default"
  )

  expect_error(
    count_grouped_distinct(42),
    "no applicable method for 'select' applied to an object of class \"c('double', 'numeric')\"",
    fixed = TRUE
  )
})

# Common utility functions to clean and annotate data ------------------------------------

# orbi_filter_satellite_peaks

test_that("orbi_filter_satellite_peaks() tests", {
  df <- suppressMessages(
    orbi_read_isox(system.file(
      "extdata",
      "testfile_dual_inlet.isox",
      package = "isoorbi"
    ))
  )

  expect_warning(
    suppressMessages(orbi_filter_satellite_peaks(df)),
    "deprecated"
  ) |>
    suppressMessages() |>
    suppressWarnings() # cascade suppressed
})

# orbi_flag_satellite_peaks
test_that("orbi_flag_satellite_peaks() tests", {
  # success
  df <- suppressMessages(
    orbi_read_isox(system.file(
      "extdata",
      "testfile_dual_inlet_new.isox",
      package = "isoorbi"
    ))
  )

  expect_true(
    suppressMessages(is.tbl(orbi_flag_satellite_peaks(orbi_simplify_isox(df))))
  )

  # failure
  expect_error(orbi_flag_satellite_peaks(), "need a `dataset` data frame")

  expect_error(
    orbi_flag_satellite_peaks(dataset = T),
    "need a `dataset` data frame"
  )

  expect_error(
    orbi_flag_satellite_peaks(dataset = df[, 1:5]),
    "`dataset` requires columns `filepath`, `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`",
    fixed = TRUE
  )

  df2 <- df |> mutate(dummy = 1) |> select(-"scan.no")

  expect_error(
    orbi_flag_satellite_peaks(dataset = df2),
    "`dataset` requires columns `filepath`, `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`",
    fixed = TRUE
  )
})

# orbi_filter_weak_isotopocules
test_that("orbi_filter_weak_isotopocules() tests", {
  df <- suppressMessages(
    orbi_read_isox(system.file(
      "extdata",
      "testfile_dual_inlet.isox",
      package = "isoorbi"
    ))
  )

  expect_warning(suppressMessages(
    orbi_filter_weak_isotopocules(df, min_percent = 5),
    "deprecated"
  )) |>
    suppressMessages() |>
    suppressWarnings() # cascade suppressed
})


# orbi_flag_weak_isotopocules
test_that("orbi_flag_weak_isotopocules() tests", {
  # failure
  expect_error(
    orbi_flag_weak_isotopocules(),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_weak_isotopocules(dataset = T),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  df <- suppressMessages(
    orbi_read_isox(system.file(
      "extdata",
      "testfile_dual_inlet.isox",
      package = "isoorbi"
    ))
  )

  expect_error(
    orbi_flag_weak_isotopocules(dataset = df),
    "`min_percent` needs to be a single number",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_weak_isotopocules(dataset = df[, 1:5]),
    "`dataset` requires columns `filepath`, `filename`, `compound`, `scan.no`, `time.min`, `isotopocule`, `ions.incremental`, `tic` and `it.ms`",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_weak_isotopocules(dataset = df[0, ]),
    "`min_percent` needs to be a single number",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_weak_isotopocules(dataset = df, min_percent = T),
    "`min_percent` needs to be a single number",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_weak_isotopocules(dataset = df, min_percent = 100),
    "`min_percent` needs to be between 0 and 90",
    fixed = TRUE
  )

  expect_true(
    suppressMessages(is.tbl(orbi_flag_weak_isotopocules(
      dataset = orbi_simplify_isox(df),
      min_percent = 1
    )))
  )

  df2 <- read.csv(file.path(base_dir, "test_files", "first10rows.csv")) |>
    select(-"scan.no")

  expect_error(orbi_flag_weak_isotopocules(dataset = df2, min_percent = 1))

  df3 <- df |>
    mutate(
      block = as.factor("block1"),
      segment = as.factor("segment2"),
      injection = as.factor("injection3")
    )

  # success
  expect_true(
    suppressMessages(is.tbl(orbi_flag_weak_isotopocules(
      dataset = df3,
      min_percent = 1
    )))
  )
})

# orbi_filter_scan_intensity

test_that("orbi_filter_scan_intensity() tests", {
  df <- suppressMessages(
    orbi_read_isox(system.file(
      "extdata",
      "testfile_dual_inlet.isox",
      package = "isoorbi"
    ))
  )

  expect_warning(
    suppressMessages(orbi_filter_scan_intensity(df, outlier_percent = 5)),
    "deprecated"
  ) |>
    suppressMessages() |>
    suppressWarnings() # cascade suppressed
})

# orbi_flag_outliers

test_that("orbi_flag_outliers() tests", {
  # failure

  expect_error(orbi_flag_outliers(), "need a `dataset` data frame")

  expect_error(
    orbi_flag_outliers(dataset = T),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  df <- read.csv(
    file.path(base_dir, "test_files", "first10rows.csv"),
    stringsAsFactors = T
  )

  expect_error(
    orbi_flag_outliers(dataset = df),
    "need to define at least one of these parameters for identifying outliers: 'agc_window', 'agc_fold_cutoff'",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_outliers(dataset = df, agc_window = T),
    "`agc_window` needs to be a vector of two numbers (low and high filter) between 0 and 100",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_outliers(dataset = df, agc_window = 1000),
    "`agc_window` needs to be a vector of two numbers (low and high filter) between 0 and 100",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_outliers(dataset = df, agc_fold_cutoff = T),
    "if provided, `agc_fold_cutoff` needs to be a single number",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_outliers(
      dataset = df,
      agc_window = c(2, 98),
      agc_fold_cutoff = 2
    ),
    "can only use one method at a time, please call this function sequentially for each of these parameters: 'agc_window', 'agc_fold_cutoff'",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_outliers(dataset = df[, 1:5]),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `tic` and `it.ms`",
    fixed = TRUE
  )

  expect_error(
    orbi_flag_outliers(dataset = df[0, ]),
    "need to define at least one of these parameters for identifying outliers: 'agc_window', 'agc_fold_cutoff'",
    fixed = TRUE
  )

  df2 <- df |> mutate(dummy = 1) |> select(-scan.no)
  expect_error(
    orbi_flag_outliers(dataset = df2, agc_window = 1),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `tic` and `it.ms`",
    fixed = TRUE
  )

  # success
  expect_true(
    suppressMessages(is.tbl(orbi_flag_outliers(
      dataset = df,
      agc_window = c(10, 90)
    )))
  )

  df3 <-
    suppressMessages(
      orbi_read_isox(system.file(
        "extdata",
        "testfile_dual_inlet.isox",
        package = "isoorbi"
      )) |>
        mutate(
          block = as.factor("block1"),
          segment = as.factor("segment2"),
          injection = as.factor("injection3")
        )
    )

  expect_true(
    suppressMessages(is.tbl(orbi_flag_outliers(
      dataset = df3,
      agc_window = c(10, 90)
    )))
  )
})

# orbi_define_basepeak()

test_that("orbi_define_basepeak() tests", {
  # failure

  expect_error(
    orbi_define_basepeak(),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  expect_error(
    orbi_define_basepeak(dataset = T),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  df <- read.csv(
    file.path(base_dir, "test_files", "first10rows.csv"),
    stringsAsFactors = T
  )

  expect_error(
    orbi_define_basepeak(dataset = df),
    "`basepeak_def` needs to be a single text value identifying the isotopocule to use as the basepeak",
    fixed = TRUE
  )

  expect_error(
    orbi_define_basepeak(dataset = df, basepeak_def = F),
    "`basepeak_def` needs to be a single text value identifying the isotopocule to use as the basepeak",
    fixed = TRUE
  )

  expect_error(
    orbi_define_basepeak(dataset = df, basepeak_def = c("M0", "123")),
    "`basepeak_def` needs to be a single text value identifying the isotopocule to use as the basepeak",
    fixed = TRUE
  )

  expect_error(
    orbi_define_basepeak(dataset = df, basepeak_def = c("ABC123")),
    "`basepeak_def` is not an isotopocule in the dataset",
    fixed = TRUE
  )

  df2 <- df |> select(-scan.no)
  expect_error(
    orbi_define_basepeak(dataset = df2, basepeak_def = "M0"),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `isotopocule`, and `ions.incremental`",
    fixed = TRUE
  )

  df3 <- df
  df3[df3 == "17O"] <- "M0"

  expect_error(
    orbi_define_basepeak(df3, basepeak_def = "M0"),
    "the M0 isotopocule exists multiple times in some scans, make sure to run orbi_flag_satellite_peaks() first",
    fixed = TRUE
  ) |>
    suppressMessages()

  df3[df3 == "M0"] <- "17O"

  suppressMessages(expect_error(
    orbi_define_basepeak(df3, basepeak_def = "M0"),
    "the 'M0' isotopocule does not exist in some scans, consider using `orbi_filter_isox()` to focus on specific file(s) and/or compound(s): \n - basepeak 'M0' is missing in 2 scans (100.0%) of compound 'HSO4-' in file 's3744'",
    fixed = TRUE
  ))

  # success
  expect_message(
    expect_true(is.tbl(orbi_define_basepeak(
      dataset = df,
      basepeak_def = "M0"
    ))),
    "setting.*denominator"
  ) |>
    suppressMessages()
})
