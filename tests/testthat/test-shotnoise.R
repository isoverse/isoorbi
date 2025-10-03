# Tests: shotnoise functions to calculate and plot shot noise in data ------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

# orbi_analyze_shot_noise
test_that("orbi_analyze_shot_noise()", {
  # errors
  orbi_analyze_shot_noise() |>
    expect_error("must be.*aggregated.*or.*data frame")
  orbi_analyze_shot_noise(mtcars) |> expect_error("requires defined basepeak")

  # test data
  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    suppressMessages()

  df_results <-
    df |>
    # define base peak
    orbi_define_basepeak("15N") |>
    suppressMessages()

  # success
  expect_message(
    out <- orbi_analyze_shot_noise(df_results),
    "analyzed the shot noise"
  )
})

# orbi_plot_shot_noise

test_that("orbi_plot_shot_noise()", {
  # failure
  expect_error(orbi_plot_shot_noise(), "shotnoise.* must be a data frame")

  expect_error(orbi_plot_shot_noise(42), "shotnoise.* must be a data frame")

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    orbi_define_basepeak(basepeak_def = "15N") |>
    orbi_analyze_shot_noise() |>
    suppressMessages()

  expect_error(
    orbi_plot_shot_noise(df, 42),
    "`x` must be a character vector, not the number 42.",
    fixed = TRUE
  )

  expect_error(
    orbi_plot_shot_noise(df, "xyz"),
    "`x` must be one of \"time.min\" or \"n_effective_ions\", not \"xyz\".",
    fixed = TRUE
  )

  expect_error(
    orbi_plot_shot_noise(df, "time.min", permil_target = 0.5, color = 42),
    "`shotnoise` requires a factor column set for `color` aesthetic",
    fixed = TRUE
  )

  expect_error(
    df |> select(-"filename") |> orbi_plot_shot_noise(),
    "column.*filename.*is missing from.*shotnoise"
  )

  expect_error(
    df |> select(-"ratio_rel_se.permil") |> orbi_plot_shot_noise(),
    "requires columns.*make sure to run.*orbi_analyze_shot_noise()"
  )

  expect_error(
    orbi_plot_shot_noise(df, "time.min", colors = 42),
    "not enough `colors` provided, 5 needed for distinguishing ratio_label",
    fixed = TRUE
  )

  # success
  vdiffr::expect_doppelganger(
    "shot noise plot 1",
    orbi_plot_shot_noise(df, "time.min")
  )
  vdiffr::expect_doppelganger(
    "shot noise plot 2",
    orbi_plot_shot_noise(df, x = "n_effective_ions")
  )
  vdiffr::expect_doppelganger(
    "shot noise plot 3",
    orbi_plot_shot_noise(df, x = "n_effective_ions", permil_target = 1)
  )
})
