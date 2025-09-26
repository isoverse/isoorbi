# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

# Common utility functions to clean and annotate data ========

## orbi_filter_satellite_peaks() =============

test_that("orbi_filter_satellite_peaks()", {
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

## orbi_flag_satellite_peaks() =============

test_that("orbi_flag_satellite_peaks()", {
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
  expect_error(
    orbi_flag_satellite_peaks(),
    "must be.*aggregated raw files.*or.*data frame"
  )

  expect_error(
    orbi_flag_satellite_peaks(dataset = T),
    "must be.*aggregated raw files.*or.*data frame"
  )

  expect_error(
    orbi_flag_satellite_peaks(dataset = df[, 1:5]),
    "columns.*are missing"
  )
})

## orbi_filter_weak_isotopocules() =============

test_that("orbi_filter_weak_isotopocules()", {
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


## orbi_flag_weak_isotopocules() =============

test_that("orbi_flag_weak_isotopocules()", {
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
    ) |>
    orbi_flag_weak_isotopocules(min_percent = 1)

  # success
  ## message
  test_that_cli(
    "orbi_flag_weak_isotopocules()",
    configs = c("plain", "fancy"),
    {
      # no weak isotopocules
      expect_snapshot(
        out <- df |>
          mutate(
            block = as.factor("block1"),
            segment = as.factor("segment2"),
            injection = as.factor("injection3")
          ) |>
          orbi_flag_weak_isotopocules(min_percent = 1)
      )

      # some weak ones
      expect_snapshot(
        out <- df |>
          filter(!(isotopocule %in% c("15N", "17O") & scan.no > 10)) |>
          orbi_flag_weak_isotopocules(min_percent = 50)
      )
    }
  ) |>
    withr::with_options(new = list(show_exec_times = FALSE))

  # ## data
  # expect_snapshot_value(
  #   orbi_flag_weak_isotopocules(dataset = df, min_percent = 1),
  #   style = "json2"
  # )
})

## orbi_filter_scan_intensity() =============

test_that("orbi_filter_scan_intensity()", {
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

## orbi_flag_outliers() =============

test_that("orbi_flag_outliers()", {
  # failure

  expect_error(orbi_flag_outliers(), "need a `dataset` data frame")

  expect_error(
    orbi_flag_outliers(dataset = T),
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

  expect_error(
    df |>
      mutate(dummy = 1) |>
      select(-"scan.no") |>
      orbi_flag_outliers(agc_fold_cutoff = 1),
    "`dataset` requires columns `filename`, `compound`, `scan.no`, `tic` and `it.ms`",
    fixed = TRUE
  )

  # success

  ## messages
  test_that_cli("orbi_flag_outliers()", configs = c("plain", "fancy"), {
    # some outliers
    expect_snapshot(out <- df |> orbi_flag_outliers(agc_window = c(10, 90)))
    expect_snapshot(out <- df |> orbi_flag_outliers(agc_fold_cutoff = 1.01))
    # no outliers
    expect_snapshot(
      out <- df |>
        filter(row_number() < 5) |>
        orbi_flag_outliers(agc_window = c(10, 90))
    )
    expect_snapshot(out <- df |> orbi_flag_outliers(agc_fold_cutoff = 2))
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))
})

## orbi_define_basepeak() =============

test_that("orbi_define_basepeak()", {
  # failure

  expect_error(
    orbi_define_basepeak(),
    "must be a data frame"
  )

  expect_error(
    orbi_define_basepeak(dataset = T),
    "must be a data frame"
  )

  # test data
  df <- read.csv(
    file.path(base_dir, "test_files", "first10rows.csv"),
    stringsAsFactors = T
  )

  expect_error(
    orbi_define_basepeak(dataset = df),
    "basepeak_def.*must be.*identifying the isotopocule"
  )

  expect_error(
    orbi_define_basepeak(dataset = df, basepeak_def = F),
    "basepeak_def.*must be.*identifying the isotopocule"
  )

  expect_error(
    orbi_define_basepeak(dataset = df, basepeak_def = c("M0", "123")),
    "basepeak_def.*must be.*identifying the isotopocule"
  )

  expect_error(
    orbi_define_basepeak(dataset = df, basepeak_def = c("ABC123")),
    "basepeak_def.*is not an isotopocule in the dataset"
  )

  expect_error(
    orbi_define_basepeak(
      dataset = df |> select(-"scan.no"),
      basepeak_def = "M0"
    ),
    "column.*scan.no.*is missing"
  )

  df |>
    mutate(isotopocule = if_else(isotopocule == "17O", "M0", isotopocule)) |>
    orbi_define_basepeak(basepeak_def = "M0") |>
    expect_error(
      "exists multiple times in some scans.*run.*orbi_flag_satellite_peaks"
    ) |>
    suppressMessages()

  df |>
    filter(isotopocule != "M0") |>
    orbi_define_basepeak(basepeak_def = "M0") |>
    expect_error(
      "does not exist in some scans.*consider.*orbi_filter_isox"
    ) |>
    suppressMessages()

  # success
  ## message
  test_that_cli("orbi_define_basepeak()", configs = c("plain", "fancy"), {
    expect_snapshot(
      out <- orbi_define_basepeak(dataset = df, basepeak_def = "M0")
    )
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))

  ## data
  expect_snapshot_value(
    orbi_define_basepeak(dataset = df, basepeak_def = "M0"),
    style = "deparse" # json2 doesn't work because of loss of precision in the ratios
  )
})
