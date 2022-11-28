# Tests: Functions to calculate ratios and stats --------------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("ratio functions")


# calculate_ratios_sem
test_that("calculate_ratios_sem() tests", {

  # success
  expect_equal(calculate_ratios_sem(ratios = c(1, 2, 3)), 0.57735026919)
  expect_type(calculate_ratios_sem(ratios = c(1, 2, 3)), "double")

  # failure

  expect_error(calculate_ratios_sem(),
               "no input vector for ratios supplied",
               fixed = TRUE)

  expect_error(calculate_ratios_sem(ratios = matrix(1, 2, 3)),
              "ratios need to be provided in a vector",
              fixed = TRUE)

  expect_error(calculate_ratios_sem(ratios = c("A", "B", "C")),
               "ratios need to be a numeric vector",
               fixed = TRUE)

  expect_error(calculate_ratios_sem(ratios = numeric()),
               "length of ratios needs to be > 1: 0",
               fixed = TRUE)

})

# calculate_ratios_gmean
test_that("calculate_ratios_gmean() tests", {

  # success
  list <- c(4, 5, 6)
  expect_type(calculate_ratios_gmean(ratios = list), "double")
  expect_equal(calculate_ratios_gmean(ratios = list), 4.9324241486609)

  # failure
  expect_error(calculate_ratios_gmean(), "input vector for ratios supplied")

  expect_error(calculate_ratios_gmean(ratios = matrix(1, 2, 3)),
               "ratios need to be provided in a vector",
               fixed = TRUE)

  expect_error(calculate_ratios_gmean(ratios = c("A", "B", "C")),
               "ratios need to be a numeric vector",
               fixed = TRUE)

  expect_error(calculate_ratios_gmean(ratios = numeric()),
               "length of ratios needs to be > 1: 0",
               fixed = TRUE)

})

# calculate_ratios_gsd
test_that("calculate_ratios_gsd() tests", {

  # success
  expect_type(calculate_ratios_gsd(ratios = c(1, 2, 3)), "double")

  # failure
  expect_error(calculate_ratios_gsd(), "no input vector for ratios supplied")

  expect_error(calculate_ratios_gsd(ratios = matrix(1, 2, 3)),
               "ratios need to be provided in a vector",
               fixed = TRUE)

  expect_error(calculate_ratios_gsd(ratios = c("A", "B", "C")),
               "ratios need to be a numeric vector",
               fixed = TRUE)

  expect_error(calculate_ratios_gsd(ratios = numeric()),
               "length of ratios needs to be > 1: 0",
               fixed = TRUE)
})

# calculate_ratios_gse
test_that("calculate_ratios_gse() tests", {

  # success
  expect_type(calculate_ratios_gse(ratios = c(4, 5, 6)), "double")

  # failure
  expect_error(calculate_ratios_gse(), "input vector for ratios supplied")

  expect_error(calculate_ratios_gse(ratios = matrix(1, 2, 3)),
               "ratios need to be provided in a vector",
               fixed = TRUE)

  expect_error(calculate_ratios_gse(ratios = c("A", "B", "C")),
               "ratios need to be a numeric vector",
               fixed = TRUE)

  expect_error(calculate_ratios_gse(ratios = numeric()),
               "length of ratios needs to be > 1: 0",
               fixed = TRUE)
})

# calculate_ratios_slope
test_that("calculate_ratios_slope() tests", {

  # success
  x <- c(0, 1, 2, 3)
  y <- c(0, 1, 2, 3)
  expect_type(calculate_ratios_slope(x, y), "double")
  expect_equal(calculate_ratios_slope(x, y), 1)

  # failure
  expect_error(calculate_ratios_slope(), "no input vector for x supplied")

  expect_error(calculate_ratios_slope(y = 1), "no input vector for x supplied")

  expect_error(calculate_ratios_slope(x = matrix(1, 2, 3), y = c(1,2,3)),
               "x needs to be a vector",
               fixed = TRUE)

  expect_error(calculate_ratios_slope(x = c("A"), y = c(1,2,3)),
               "x needs to be a numeric vector",
               fixed = TRUE)

  expect_error(calculate_ratios_slope(x = numeric(), y = c(1,2,3)),
               "length of x needs to be > 1: 0",
               fixed = TRUE)

  expect_error(calculate_ratios_slope(x = 1), "no input vector for y supplied")

  expect_error(calculate_ratios_slope(x = c(1,2,3), y = matrix(1, 2, 3)),
               "y needs to be a vector",
               fixed = TRUE)

  expect_error(calculate_ratios_slope(x = c(1,2,3), y = c("A")),
               "y needs to be a numeric vector",
               fixed = TRUE)
  #
  expect_error(calculate_ratios_slope(x = c(1,2,3), y = c(1)),
               "length of y needs to be > 1: 1",
               fixed = TRUE)

  expect_error(calculate_ratios_slope(x = c(1,2,3), y = c(1,2,3,4)),
               "length of x and y need to be equal",
               fixed = TRUE)


})

# calculate_weighted.vector.sum
test_that("calculate_weighted.vector.sum() tests", {

  # success
  x <- c(2, 4, 6)
  y <- c(3, 5, 7)
  expect_type(calculate_ratios_weighted_sum(x, y), "double")

  # failure
  expect_error(calculate_ratios_weighted_sum(), "no input vector for x supplied")

  expect_error(calculate_ratios_weighted_sum(x = c("A"), y = c(1,2,3)),
               "x needs to be a numeric vector",
               fixed = TRUE)

  expect_error(calculate_ratios_weighted_sum(x = numeric(), y = c(1,2,3)),
               "length of x needs to be > 1: 0",
               fixed = TRUE)

  expect_error(calculate_ratios_weighted_sum(x = c(1,2,3), y = c("A")),
               "y needs to be a numeric vector",
               fixed = TRUE)

  expect_error(calculate_ratios_weighted_sum(x = c(1,2,3), y = numeric()),
               "length of y needs to be > 1: 0",
               fixed = TRUE)

  expect_error(calculate_ratios_weighted_sum(x = c(1,2,3), y = c(1,2,3,4)),
               "length of x and y need to be equal",
               fixed = TRUE)

})

# orbi_calculate_ratios
test_that("orbi_calculate_ratios() tests", {

  a <- 1:10
  b <- 1:10

  # success
  expect_equal(orbi_calculate_ratios(a, b, "mean"), 1)
  expect_equal(orbi_calculate_ratios(a, b, "sum"), 1)
  expect_equal(orbi_calculate_ratios(a, b, "slope"), 1)
  expect_equal(orbi_calculate_ratios(a, b, "geometric_mean"), 1)
  expect_equal(orbi_calculate_ratios(a, b, "weighted_sum"), 1)
  expect_equal(orbi_calculate_ratios(a, b, "median"), 1)

  # failure
  expect_error(
    orbi_calculate_ratios(a, b, "median2"),
    "`ratio_method` has to be `mean`, `sum`, `median`, `geometric_mean`, `slope` or `weighted_sum`"
  )

  expect_error(orbi_calculate_ratios(), "no input for numerator supplied")
})

