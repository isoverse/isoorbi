# utility functions ====

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("utils")



  # orbi_filter_weak_isotopocules
test_that("orbi_filter_weak_isotopocules() tests", {
  #success
  # ...
  #failure
  expect_error(orbi_filter_weak_isotopocules(), "no dataset supplied")
})

  # orbi_filter_satellitePeaks
test_that("orbi_filter_satellite_peaks() tests", {
  #succcess
  df <- orbi_read_isox(system.file("extdata", "testfile_DualInlet_small.isox", package = "isoorbi"))
  expect_true(is.tbl(orbi_filter_satellite_peaks(orbi_simplify_isox(df))))
  #failure
  expect_error(orbi_filter_satellite_peaks(), "no dataset supplied")
})

  # orbi_filter_TICxIT
test_that("orbi_filter_scan_intensity() tests", {
  #success
  #failure
  expect_error(orbi_filter_scan_intensity(), "no dataset supplied")
})

  # orbi_filter_isox
test_that("orbi_filter_isox() tests",{
  #success
  #failure
  expect_error(orbi_filter_isox(), "no dataset supplied")
})

  # calculate_se
test_that("calculate_se() tests", {
  #success
  expect_equal(calculate_se(c(1,2,3)), 0.57735026919)
  expect_type(calculate_se(c(1,2,3)), "double")

  #failure
  expect_error(calculate_se(), "input vector for x supplied")
})

  # calculate_gmean
test_that("calculate_gmean() tests", {
  #success
  list<- c(4,5,6)
  expect_type(calculate_gmean(list), "double")
  expect_equal(calculate_gmean(list), 4.9324241486609)

  #failure
  expect_error(calculate_gmean(), "input vector for x supplied")
})

  # calculate_gsd
test_that("calculate_gsd() tests", {
  #success
  expect_type(calculate_gsd(c(1,2,3)), "double")

  #failure
  expect_error(calculate_gsd(), "input vector for x supplied")
})

  # calculate_gse
test_that("calculate_gse() tests", {
  #success
  expect_type(calculate_gse(c(4,5,6)), "double")

  #failure
  expect_error(calculate_gse(), "input vector for x supplied")
})

  # calculate_slope
test_that("calculate_slope() tests", {
  #success
  x <- c(0,1,2,3)
  y <- c(0,1,2,3)
  expect_type(calculate_slope(x,y), "double")
  expect_equal(calculate_slope(x,y), 1)

  #failure
  expect_error(calculate_slope(), "input vector for x supplied")

})

  # calculate_weighted.vector.sum
test_that("calculate_weighted.vector.sum() tests", {
  #success
  x<- c(2,4,6)
  y<- c(3,5,7)
  expect_type(calculate_weighted_sum(x,y), "double")
  #failure
  expect_error(calculate_weighted_sum(), "input vector for x supplied")
})

  # orbi_calculate_ratio
test_that("orbi_calculate_ratio() tests", {
  #success
  a<- 1:10
  b<- 1:10

    #mean
  expect_equal(orbi_calculate_ratio(a,b,"mean"), 1)
    #sum
  expect_equal(orbi_calculate_ratio(a,b,"sum"), 1)
    #slope
  expect_equal(orbi_calculate_ratio(a,b,"slope"),1)
    #geometric_mean
  expect_equal(orbi_calculate_ratio(a,b,"geometric_mean"),1)
    #weighted_sum
  expect_equal(orbi_calculate_ratio(a,b,"weighted_sum"),1)
    #median
  expect_equal(orbi_calculate_ratio(a,b,"median"), 1)

  expect_equal(orbi_calculate_ratio(a,b,"median2"), "`ratio_method` has to be `mean`, `sum`, `median`, `geometric_mean`, `slope` or `weighted_sum`")

  #failure
  expect_error(orbi_calculate_ratio(), "no input for numerator supplied")

})

