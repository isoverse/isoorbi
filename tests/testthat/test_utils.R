# utility functions ====

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("utils")



  # orbi_filter_rare
test_that("orbi_filter_rare() tests", {
  #success
  # ...
  #failure
  expect_error(orbi_filter_rare(), "argument \"data\" is missing, with no default")
})

  # orbi_filter_rare
test_that("orbi_filter_rare() tests", {
  #succcess
  df <- orbi_read_isox(system.file("extdata", "testfile_DualInlet_small.isox", package = "isoorbi"))
  expect_true(is.tbl(orbi_filter_satellitePeaks(orbi_simplify_isox(df))))
  #failure
  expect_error(orbi_filter_satellitePeaks(), "argument \"df\" is missing, with no default")
})

  # orbi_filter_TICxIT
test_that("orbi_filter_TICxIT() tests", {
  #success
  #failure
  expect_error(orbi_filter_TICxIT(), "argument \"df\" is missing, with no default")
})

  # orbi_filter_isox
test_that("orbi_filter_isox() tests",{
  #success
  #failure
  expect_error(orbi_filter_isox(), "argument \"dataset\" is missing, with no default")
})

  # calculate_se
test_that("calculate_se() tests", {
  #success
  expect_equal(calculate_se(c(1,2,3)), 0.57735026919)
  expect_type(calculate_se(c(1,2,3)), "double")

  #failure
  expect_error(calculate_se(), "argument \"x\" is missing, with no default")
})

  # calculate_gmean
test_that("calculate_gmean() tests", {
  #success
  list<- c(4,5,6)
  expect_type(calculate_gmean(list), "double")
  expect_equal(calculate_gmean(list), 4.9324241486609)

  #failure
  expect_error(calculate_gmean(), "argument \"x\" is missing, with no default")
})

  # calculate_gsd
test_that("calculate_gsd() tests", {
  #success
  expect_type(calculate_gsd(c(1,2,3)), "double")

  #failure
  expect_error(calculate_gsd(), "argument \"x\" is missing, with no default")
})

  # calculate_gse
test_that("calculate_gse() tests", {
  #success
  expect_type(calculate_gse(c(4,5,6)), "double")

  #failure
  expect_error(calculate_gse(), "argument \"x\" is missing, with no default")
})

  # calculate_slope
test_that("calculate_slope() tests", {
  #success
  x <- c(0,1,2,3)
  y <- c(0,1,2,3)
  expect_type(calculate_slope(x,y), "double")
  expect_equal(calculate_slope(x,y), 1)

  #failure
  expect_error(calculate_slope(), "argument \"x\" is missing, with no default")

})

  # calculate_weighted.vector.sum
test_that("calculate_weighted.vector.sum() tests", {
  #success
  x<- c(2,4,6)
  y<- c(3,5,7)
  expect_type(calculate_weighted.vector.sum(x,y), "double")
  #failure
  expect_error(calculate_weighted.vector.sum(), "argument \"x\" is missing, with no default")
})

  # orbi_calculate_ratio
test_that("orbi_calculate_ratio() tests", {
  #success
  a<- c(1)
  b<- c(4)
    #mean
  expect_equal(orbi_calculate_ratio(a,b,"mean"), 0.25)
    #sum
  expect_equal(orbi_calculate_ratio(a,b,"sum"), 0.25)
    #slope
  expect_equal(orbi_calculate_ratio(a,b,"slope"), 0.25)
    #geometric.mean
  expect_equal(orbi_calculate_ratio(a,b,"geometric.mean"), 0.25)
    #weighted.vector.sum
  expect_equal(orbi_calculate_ratio(a,b,"weighted.vector.sum"), 0.25)
    #median
  expect_equal(orbi_calculate_ratio(a,b,"median"), 0.25)

  #failure
  expect_error(orbi_calculate_ratio(), "the condition has length > 1")

})
#
#   # make.xls
# test_that("make.xls() tests", {
#   #success
#   #...
#   #failure
#   expect_error(make.xls(), "")
# })
#
#   # make.xls.onetab
# test_that("make.xls.onetab() tests", {
#   #success
#   #...
#   #failure
#   expect_error(make.xls.onetab(), "")
# })
