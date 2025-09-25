test_that("orbi_identify_isotopocules()", {
  # errors
  orbi_identify_isotopocules() |>
    expect_error("must be.*aggregated raw files.*or.*peaks")
  orbi_identify_isotopocules(42) |>
    expect_error("must be.*aggregated raw files.*or.*peaks")
  orbi_identify_isotopocules(tibble()) |>
    expect_error("must be.*csv.*tsv.*xlsx.*or.*isotopocules")
  orbi_identify_isotopocules(tibble(), 42) |>
    expect_error("must be.*csv.*tsv.*xlsx.*or.*isotopocules")
  test_xlsx <- file.path(tempdir(), "test.xlsx")
  cat("DNE", file = test_xlsx)
  orbi_identify_isotopocules(tibble(), test_xlsx) |>
    expect_error("something went wrong reading")
  test_csv <- file.path(tempdir(), "test.csv")
  cat("DNE", file = test_csv)
  orbi_identify_isotopocules(tibble(), test_csv) |>
    expect_error("something went wrong reading")
  test_tsv <- file.path(tempdir(), "test.tsv")
  cat("DNE", file = test_tsv)
  orbi_identify_isotopocules(tibble(), test_tsv) |>
    expect_error("something went wrong reading")

  ## columns
  orbi_identify_isotopocules(tibble(), tibble()) |>
    expect_error("could not identify.*isotopocule")
  orbi_identify_isotopocules(tibble(), tibble(isotopocule = 1)) |>
    expect_error("could not identify.*mz")
  orbi_identify_isotopocules(tibble(), tibble(isotopologue = 1, mass = 1)) |>
    expect_error("could not identify.*tolerance")
  orbi_identify_isotopocules(
    tibble(),
    tibble(isotopolog = 1, Mass = 1, `Tolerance [mmu]` = 1)
  ) |>
    expect_error("could not identify.*charge")

  # example tibble
  isotopologs <- tibble(
    compound = "nitrate",
    isotopolog = c("M0", "15N", "17O", "18O"),
    mass = c(61.9878, 62.9850, 62.9922, 63.9922),
    tolerance = 1,
    charge = 1
  )

  # missing columns in peaks
  orbi_identify_isotopocules(tibble(), isotopologs) |>
    expect_error("columns.*are missing")

  # test proper values
  peaks <- tibble(
    uidx = 1,
    scan.no = rep(c(1, 2, 3), each = 4),
    mzMeasured = c(
      isotopologs$mass - 0.0005,
      c(isotopologs$mass[-1], isotopologs$mass[2] + 0.0003) + 0.0002,
      c(isotopologs$mass[1:3], isotopologs$mass[4] + 0.0012)
    ),
    intensity = seq_along(mzMeasured)
  )

  # isotopocules too close together
  orbi_identify_isotopocules(
    peaks,
    isotopologs |> dplyr::mutate(mass = mass[1])
  ) |>
    expect_error("overlapping tolerance")

  # successful return value
  test_that_cli("orbi_identify_isotopocules()", configs = c("plain", "fancy"), {
    expect_snapshot(out <- orbi_identify_isotopocules(peaks, isotopologs))
    expect_snapshot_value(out, style = "json2")
  })

  # test with file based isotopologs
  write.csv(isotopologs, file = test_csv, row.names = FALSE)
  write.table(isotopologs, file = test_tsv, sep = "\t", row.names = FALSE)
  openxlsx::write.xlsx(isotopologs, file = test_xlsx)

  expect_equal(
    orbi_identify_isotopocules(peaks, isotopologs),
    orbi_identify_isotopocules(peaks, test_csv)
  ) |>
    suppressMessages()

  expect_equal(
    orbi_identify_isotopocules(peaks, isotopologs),
    orbi_identify_isotopocules(peaks, test_tsv)
  ) |>
    suppressMessages()

  expect_equal(
    orbi_identify_isotopocules(peaks, isotopologs),
    orbi_identify_isotopocules(peaks, test_xlsx)
  ) |>
    suppressMessages()

  # cleanup
  unlink(test_xlsx)
  unlink(test_csv)
  unlink(test_tsv)
}) |>
  withr::with_options(new = list(show_exec_times = FALSE))
