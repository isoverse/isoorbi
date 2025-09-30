# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

# example tibble
isotopologs <- tibble(
  compound = "nitrate",
  isotopolog = c("M0", "15N", "17O", "18O"),
  mass = c(61.9878, 62.9850, 62.9922, 63.9922),
  tolerance = 1,
  charge = 1
)

# example peaks
peaks <- tibble(
  uidx = 1,
  scan.no = rep(c(1, 2, 3), each = 4),
  tic = scan.no,
  it.ms = scan.no,
  mzMeasured = c(
    isotopologs$mass - 0.0005,
    c(isotopologs$mass[-1], isotopologs$mass[2] + 0.0003) + 0.0002,
    c(isotopologs$mass[1:3], isotopologs$mass[4] + 0.0012)
  ),
  intensity = seq_along(mzMeasured)
) |>
  orbi_identify_isotopocules(isotopologs) |>
  suppressMessages()

# eample agg data
agg_data <- structure(
  list(
    file_info = tibble(uidx = 1, filename = "testfile"),
    scans = peaks |>
      filter(!is.na(.data$tic)) |>
      select("uidx", "scan.no", "tic", "it.ms") |>
      distinct(),
    peaks = peaks
  ),
  class = "orbi_aggregated_data"
)

# example test file
test_file <-
  system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  ) |>
  orbi_read_isox() |>
  orbi_simplify_isox() |>
  suppressMessages() |>
  select(-"filepath")

test_that("orbi_filter_satellite_peaks()", {
  # DEPRECATED
  orbi_filter_satellite_peaks(peaks) |>
    expect_warning("deprecated") |>
    suppressMessages()
})

test_that("orbi_flag_satellite_peaks()", {
  # errors
  expect_error(
    orbi_flag_satellite_peaks(),
    "must be.*aggregated raw files.*or.*data frame"
  )

  # success
  test_that_cli("orbi_flag_satellite_peaks()", configs = c("plain", "fancy"), {
    # example dataset - test agg data vs tibble
    expect_snapshot(agg_out <- orbi_flag_satellite_peaks(agg_data))
    expect_snapshot(agg_out)
    expect_snapshot_value(agg_out$peaks, style = "json2")
    expect_equal(agg_out$peaks, orbi_flag_satellite_peaks(peaks)) |>
      suppressMessages()

    # test order of operations to be equal in outcome
    expect_equal(
      agg_out |>
        orbi_filter_isotopocules() |>
        orbi_get_data(file_info = NULL, peaks = everything()),
      peaks |> orbi_filter_isotopocules() |> orbi_flag_satellite_peaks()
    ) |>
      suppressMessages()

    # test real file
    expect_snapshot(out <- orbi_flag_satellite_peaks(test_file))
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))
})

test_that("orbi_filter_weak_isotopocules()", {
  # DEPRECATED
  orbi_filter_weak_isotopocules(peaks, min_percent = 5) |>
    suppressMessages() |>
    expect_warning("deprecated")
})

test_that("orbi_flag_weak_isotopocules()", {
  # errors
  orbi_flag_weak_isotopocules() |>
    expect_error("must be.*aggregated raw files.*or.*data frame")

  orbi_flag_weak_isotopocules(peaks) |>
    expect_error("min_percent.*must be a single number")

  # success
  test_that_cli(
    "orbi_flag_weak_isotopocules()",
    configs = c("plain", "fancy"),
    {
      # example dataset - test agg data vs tibble
      expect_snapshot(agg_out <- orbi_flag_weak_isotopocules(agg_data, 90))
      expect_snapshot(agg_out)
      expect_snapshot_value(agg_out$peaks, style = "json2")
      expect_equal(agg_out$peaks, orbi_flag_weak_isotopocules(peaks, 90)) |>
        suppressMessages()

      # test order of operations to be equal in outcome
      expect_equal(
        agg_out |>
          orbi_filter_isotopocules() |>
          orbi_get_data(file_info = NULL, peaks = everything()),
        peaks |> orbi_filter_isotopocules() |> orbi_flag_weak_isotopocules(90)
      ) |>
        suppressMessages()

      # test real file
      expect_snapshot(out <- orbi_flag_weak_isotopocules(test_file, 90))
      expect_snapshot(out <- orbi_flag_weak_isotopocules(test_file, 99.999))

      # FIXME: add tests with segmentation
    }
  ) |>
    withr::with_options(new = list(show_exec_times = FALSE))
})

test_that("orbi_filter_scan_intensity()", {
  # DEPRECATED
  orbi_filter_scan_intensity(peaks, outlier_percent = 5) |>
    suppressMessages() |>
    expect_warning("deprecated")
})

test_that("orbi_flag_outliers()", {
  # errors
  orbi_flag_outliers() |>
    expect_error("must be.*aggregated raw files.*or.*data frame")

  orbi_flag_outliers(peaks) |>
    expect_error(
      "need to define at least one of these parameters for identifying outliers:.*agc_window.*agc_fold_cutoff'"
    )

  orbi_flag_outliers(peaks, agc_window = T) |>
    expect_error(
      "agc_window.*needs to be a vector of two numbers.*between 0 and 100"
    )

  orbi_flag_outliers(peaks, agc_fold_cutoff = T) |>
    expect_error("agc_fold_cutoff.*needs to be a single number")

  orbi_flag_outliers(peaks, agc_window = c(2, 98), agc_fold_cutoff = 2) |>
    expect_error(
      "can only use one method at a time, please call this function sequentially for each of these parameters"
    )

  # success
  test_that_cli("orbi_flag_outliers()", configs = c("plain", "fancy"), {
    # agc window
    expect_snapshot(
      agg_out <- agg_data |> orbi_flag_outliers(agc_window = c(10, 90))
    )
    expect_snapshot(agg_out)
    expect_snapshot_value(agg_out$scans, style = "json2")
    expect_equal(
      agg_out$scans,
      peaks |>
        orbi_flag_outliers(agc_window = c(10, 90)) |>
        select("uidx":"it.ms", "is_outlier", "outlier_type") |>
        filter(!is.na(tic)) |>
        distinct()
    ) |>
      suppressMessages()

    # agc_fold_cutoff
    expect_snapshot(
      agg_out <- agg_data |> orbi_flag_outliers(agc_fold_cutoff = 2)
    )

    # real file test
    expect_snapshot(
      out <- test_file |> orbi_flag_outliers(agc_window = c(10, 90))
    )
    expect_snapshot(out <- test_file |> orbi_flag_outliers(agc_fold_cutoff = 2))

    # FIXME: add tests with blocks
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))
})

test_that("orbi_define_basepeak()", {
  # failure

  expect_error(
    orbi_define_basepeak(),
    "must be.*aggregated.*or.*data frame"
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
      "does not exist in some scans"
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
  ) |>
    suppressMessages()
})
