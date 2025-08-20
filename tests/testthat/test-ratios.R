# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

# calculate_ratios_sem() ============

test_that("calculate_ratios_sem()", {
  # success
  expect_equal(calculate_ratios_sem(ratios = c(1, 2, 3)), 0.57735026919)
  expect_type(calculate_ratios_sem(ratios = c(1, 2, 3)), "double")

  # failure

  expect_error(
    calculate_ratios_sem(),
    "no input vector for `ratios` supplied",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_sem(ratios = matrix(1, 2, 3)),
    "`ratios` need to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_sem(ratios = c("A", "B", "C")),
    "`ratios` need to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_sem(ratios = numeric()),
    "length of `ratios` needs to be > 1",
    fixed = TRUE
  )
})

# calculate_ratios_gmean() ============

# calculate_ratios_gmean
test_that("calculate_ratios_gmean()", {
  # success
  list <- c(4, 5, 6)
  expect_type(calculate_ratios_gmean(ratios = list), "double")
  expect_equal(calculate_ratios_gmean(ratios = list), 4.9324241486609)

  # failure
  expect_error(calculate_ratios_gmean(), "input vector for `ratios` supplied")

  expect_error(
    calculate_ratios_gmean(ratios = matrix(1, 2, 3)),
    "`ratios` need to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_gmean(ratios = c("A", "B", "C")),
    "`ratios` need to be provided as a numeric vector",
    fixed = TRUE
  )
})

# calculate_ratios_gsd() ============

# calculate_ratios_gsd
test_that("calculate_ratios_gsd()", {
  # success
  expect_type(calculate_ratios_gsd(ratios = c(1, 2, 3)), "double")

  # failure
  expect_error(calculate_ratios_gsd(), "no input vector for `ratios` supplied")

  expect_error(
    calculate_ratios_gsd(ratios = matrix(1, 2, 3)),
    "`ratios` need to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_gsd(ratios = c("A", "B", "C")),
    "`ratios` need to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_gsd(ratios = numeric()),
    "length of `ratios` needs to be > 1",
    fixed = TRUE
  )
})

# calculate_ratios_gse() ============

# calculate_ratios_gse
test_that("calculate_ratios_gse()", {
  # success
  expect_type(calculate_ratios_gse(ratios = c(4, 5, 6)), "double")

  # failure
  expect_error(calculate_ratios_gse(), "input vector for `ratios` supplied")

  expect_error(
    calculate_ratios_gse(ratios = matrix(1, 2, 3)),
    "`ratios` need to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_gse(ratios = c("A", "B", "C")),
    "`ratios` need to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_gse(ratios = numeric()),
    "length of `ratios` needs to be > 1",
    fixed = TRUE
  )
})

# calculate_ratios_slope() ============

# calculate_ratios_slope
test_that("calculate_ratios_slope()", {
  # success
  x <- c(0, 1, 2, 3)
  y <- c(0, 1, 2, 3)
  expect_type(calculate_ratios_slope(x, y), "double")
  expect_equal(calculate_ratios_slope(x, y), 1)

  # failure
  expect_error(calculate_ratios_slope(), "no input vector for `x` supplied")

  expect_error(
    calculate_ratios_slope(y = 1),
    "no input vector for `x` supplied"
  )

  expect_error(
    calculate_ratios_slope(x = matrix(1, 2, 3), y = c(1, 2, 3)),
    "`x` needs to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_slope(x = c("A"), y = c(1, 2, 3)),
    "`x` needs to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_slope(x = numeric(), y = c(1, 2, 3)),
    "length of `x` needs to be > 1",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_slope(x = 1),
    "no input vector for `y` supplied"
  )

  expect_error(
    calculate_ratios_slope(x = c(1, 2, 3), y = matrix(1, 2, 3)),
    "`y` needs to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_slope(x = c(1, 2, 3), y = c("A")),
    "`y` needs to be provided as a numeric vector",
    fixed = TRUE
  )
  #
  expect_error(
    calculate_ratios_slope(x = c(1, 2, 3), y = c(1)),
    "length of `y` needs to be > 1",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_slope(x = c(1, 2, 3), y = c(1, 2, 3, 4)),
    "`x` and `y` need to be vectors of equal length",
    fixed = TRUE
  )
})

# calculate_ratios_weighted_sum() ============

# calculate_ratios_weighted_sum
test_that("calculate_ratios_weighted_sum()", {
  # success
  x <- c(2, 4, 6)
  y <- c(3, 5, 7)

  expect_type(calculate_ratios_weighted_sum(x, y), "double")

  # failure
  expect_error(
    calculate_ratios_weighted_sum(),
    "no input vector for `x` supplied"
  )

  expect_error(
    calculate_ratios_weighted_sum(x = matrix(1, 2, 3), y = c(1, 2, 3)),
    "`x` needs to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_weighted_sum(x = c("A"), y = c(1, 2, 3)),
    "`x` needs to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_weighted_sum(x = numeric(), y = c(1, 2, 3)),
    "length of `x` needs to be > 1",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_weighted_sum(x = c(1, 2, 3)),
    "no input vector for `y` supplied"
  )

  expect_error(
    calculate_ratios_weighted_sum(x = c(1, 2, 3), y = matrix(1, 2, 3)),
    "`y` needs to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_weighted_sum(x = c(1, 2, 3), y = c("A")),
    "`y` needs to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_weighted_sum(x = c(1, 2, 3), y = numeric()),
    "length of `y` needs to be > 1",
    fixed = TRUE
  )

  expect_error(
    calculate_ratios_weighted_sum(x = c(1, 2, 3), y = c(1, 2, 3, 4)),
    "`x` and `y` need to be vectors of equal length",
    fixed = TRUE
  )
})

# orbi_calculate_summarized_ratio() ============

# orbi_calculate_ratio
test_that("orbi_calculate_summarized_ratio()", {
  a <- 1:10
  b <- 1:10

  # success
  expect_equal(orbi_calculate_summarized_ratio(a, b, "mean"), 1)
  expect_equal(orbi_calculate_summarized_ratio(a, b, "sum"), 1)
  expect_equal(orbi_calculate_summarized_ratio(a, b, "slope"), 1)
  expect_equal(orbi_calculate_summarized_ratio(a, b, "geometric_mean"), 1)
  expect_equal(orbi_calculate_summarized_ratio(a, b, "weighted_sum"), 1)
  expect_equal(orbi_calculate_summarized_ratio(a, b, "median"), 1)

  a <- 1
  b <- 1
  expect_equal(orbi_calculate_summarized_ratio(a, b, "direct"), 1)

  # failure
  expect_error(
    orbi_calculate_summarized_ratio(
      numerator = vector("numeric", 2),
      denominator = vector("numeric", 1),
      "direct"
    ),
    "`numerator` and `denominator` must be vectors of equal length"
  )

  expect_error(
    orbi_calculate_summarized_ratio(),
    "no input for `numerator` supplied",
    fixed = TRUE
  )

  expect_error(
    orbi_calculate_summarized_ratio(numerator = a),
    "no input for `denominator` supplied",
    fixed = TRUE
  )

  expect_error(
    orbi_calculate_summarized_ratio(
      numerator = as.character(a),
      denominator = b
    ),
    "`numerator` needs to be provided as a numeric vector",
    fixed = TRUE
  )

  expect_error(
    orbi_calculate_summarized_ratio(
      numerator = a,
      denominator = as.character(b)
    ),
    "`denominator` needs to be provided as a numeric vector",
    fixed = TRUE
  )
})

# orbi_calculate_ratios() ============

# orbi_calculate_ratios
test_that("orbi_calculate_ratios()", {
  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
    suppressMessages()
  df_results <-
    df |>
    # define base peak
    orbi_define_basepeak("15N") |>
    suppressMessages()

  # errors
  expect_error(
    orbi_calculate_ratios(),
    "need a `dataset` data frame",
    fixed = TRUE
  )

  df_results2 <- select(df_results, -"basepeak_ions")

  expect_error(
    orbi_calculate_ratios(df_results2),
    "`dataset` requires defined basepeak (columns `basepeak` and `basepeak_ions`), make sure to run `orbi_define_basepeak()` first",
    fixed = TRUE
  )

  # deprecation warning
  expect_warning(orbi_calculate_ratios(df_results), "obsolete") |>
    suppressMessages()
})

# orbi_summarize_results() ============

test_that("orbi_summarize_results()", {
  # test data
  df <- orbi_read_isox(system.file(
    "extdata",
    "testfile_dual_inlet.isox",
    package = "isoorbi"
  )) |>
    orbi_simplify_isox() |>
    orbi_define_basepeak(basepeak_def = "15N") |>
    suppressMessages()

  # errors
  orbi_summarize_results() |> expect_error("must be a data frame")
  orbi_summarize_results(df) |> expect_error("must be a valid method")
  orbi_summarize_results(df, "foo") |>
    expect_error(
      "ratio_method.* must be one of.*mean.*sum.*median.*geometric_mean.*slope.*weighted_sum"
    )
  orbi_summarize_results(mtcars, "sum") |>
    expect_error("columns.*missing")

  # capture success message
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot({
      res <- orbi_summarize_results(df, ratio_method = "sum")
    })
  })

  # capture sucess data without blocks
  df |>
    orbi_summarize_results(ratio_method = "sum") |>
    select(-"filename") |>
    expect_snapshot_value(style = "json2")

  # capture success data with blocks and a different method
  df |>
    mutate(
      block = as.factor("block1"),
      segment = as.factor("segment2"),
      injection = as.factor("injection3"),
      sample_name = as.factor("sample_name4")
    ) |>
    orbi_summarize_results(ratio_method = "mean") |>
    expect_snapshot_value(style = "json2")
})
