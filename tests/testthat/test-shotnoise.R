# Tests: shotnoise functions to calculate and plot shot noise in data ------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

# orbi_analyze_shot_noise
test_that("orbi_analyze_shot_noise() tests", {

  # failure
  expect_error(orbi_analyze_shot_noise(),
               "need a `dataset` data frame",
               fixed = TRUE)

  expect_error(orbi_analyze_shot_noise(42),
               "need a `dataset` data frame",
               fixed = TRUE)

  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")) |>
    suppressMessages()
  
  df_2 <- subset(df, select = -filename)

  expect_error(orbi_analyze_shot_noise(df_2),
               "`dataset` requires columns `filename`, `compound` and `isotopocule`",
               fixed = TRUE)

  df_results <-
    df |>
    # define base peak
    orbi_define_basepeak("15N") |>
    suppressMessages()
  df_results2 <- subset(df_results, select = -basepeak_ions)

  expect_error(orbi_calculate_ratios(df_results2),
               "`dataset` requires defined basepeak (columns `basepeak` and `basepeak_ions`), make sure to run `orbi_define_basepeak()` first",
               fixed = TRUE)

  # success

  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")) |> orbi_simplify_isox() |> orbi_define_basepeak(basepeak_def = "15N") |>
    suppressMessages()

  expect_true(is.tbl(orbi_analyze_shot_noise(df))) |>
    suppressMessages()

  expect_type(orbi_analyze_shot_noise(df), "list") |>
    suppressMessages()

})

# orbi_plot_shot_noise

test_that("orbi_plot_shot_noise() tests", {

  # failure
  expect_error(orbi_plot_shot_noise(),
               "need a `shotnoise` data frame",
               fixed = TRUE)

  expect_error(orbi_plot_shot_noise(42),
               "need a `shotnoise` data frame",
               fixed = TRUE)

  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")) |>
    orbi_simplify_isox() |> orbi_define_basepeak(basepeak_def = "15N") |>
    orbi_analyze_shot_noise() |>
    suppressMessages()

  expect_error(orbi_plot_shot_noise(df, 42),
               "`x` must be a character vector, not the number 42.",
               fixed = TRUE)

  expect_error(orbi_plot_shot_noise(df, "xyz"),
               "`x` must be one of \"time.min\" or \"n_effective_ions\", not \"xyz\".",
               fixed = TRUE)

  expect_error(orbi_plot_shot_noise(df, "time.min", permil_target = 0.5, color = 42),
               "`shotnoise` requires a factor column set for `color` aesthetic",
               fixed = TRUE)

  df_2 <- subset(df, select = -filename)

  expect_error(orbi_plot_shot_noise(df_2),
               "`shotnoise` requires columns `filename`, `compound`, `isotopocule` and `basepeak`",
               fixed = TRUE)

  df_2 <- subset(df, select = -ratio_rel_se.permil)

  expect_error(orbi_plot_shot_noise(df_2),
               "`shotnoise` requires columns `ratio_rel_se.permil` and `shot_noise.permil`, make sure to run `orbi_analyze_shot_noise()` first",
               fixed = TRUE)

  expect_error(orbi_plot_shot_noise(df, "time.min", colors = 42),
               "not enough `colors` provided, 5 needed for distinguishing ratio_label",
               fixed = TRUE)

  # success
  suppressPackageStartupMessages(library(ggplot2))
  expect_type(orbi_plot_shot_noise(df, "time.min"), "list")

  fig <- orbi_plot_shot_noise(df, x = "n_effective_ions")
  expect_equal(fig$facet$vars(), character(0))

  fig2 <- orbi_plot_shot_noise(df, x = "n_effective_ions", permil_target = 1)
  expect_equal(length(fig2$layers), 5)
})
