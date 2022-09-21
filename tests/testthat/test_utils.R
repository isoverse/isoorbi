# core functions ====

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("core")



  # remove.rare
test_that("remove.rare() tests", {
  #success
  # ...
  #failure
  expect_error(remove.rare(), "argument \"data\" is missing, with no default")
})

#   # remove.satellitePeaks
# test_that("remove.satellitePeaks() tests", {
#   #succcess
#   df <- iso_read_isox_file(system.file("extdata", "testfile_DualInlet_small.isox", package = "isoorbi"))
#   expect_true(is.tbl(remove.satellitePeaks(iso_simplify_isox_file(df))))
#   #failure
#   expect_error(remove.satellitePeaks(), "argument \"df\" is missing, with no default")
# })
#
#   # remove.ticxit
# test_that("remove.ticxit() tests", {
#   #success
#   #failure
#   expect_error(remove.ticxit(), "argument \"df\" is missing, with no default")
# })
#
#   # iso_filter_isox_file
# test_that("iso_filter_isox_file() tests",{
#   #success
#   #failure
#   expect_error(iso_filter_isox_file(), "argument \"dataset\" is missing, with no default")
# })
#
#   # se
# test_that("se() tests", {
#   #success
#   expect_equal(se(c(1,2,3)), 0.57735026919)
#   expect_type(se(c(1,2,3)), "double")
#
#   #failure
#   expect_error(se(), "argument \"x\" is missing, with no default")
# })
#
#   # gmean
# test_that("gmean() tests", {
#   #success
#   list<- c(4,5,6)
#   expect_type(gmean(list), "double")
#   expect_equal(gmean(list), 4.9324241486609)
#
#   #failure
#   expect_error(gmean(), "argument \"x\" is missing, with no default")
# })
#
#   # gsd
# test_that("gsd() tests", {
#   #success
#   expect_type(gsd(c(1,2,3)), "double")
#
#   #failure
#   expect_error(gsd(), "argument \"x\" is missing, with no default")
# })
#
#   # gse
# test_that("gse() tests", {
#   #success
#   expect_type(gse(c(4,5,6)), "double")
#
#   #failure
#   expect_error(gse(), "argument \"x\" is missing, with no default")
# })
#
#   # slope
# test_that("slope() tests", {
#   #success
#   x <- c(0,1,2,3)
#   y <- c(0,1,2,3)
#   expect_type(slope(x,y), "double")
#   expect_equal(slope(x,y), 1)
#
#   #failure
#   expect_error(slope(), "argument \"x\" is missing, with no default")
#
# })
#
#   # weighted.vector.sum
# test_that("weighted.vector.sum() tests", {
#   #success
#   x<- c(2,4,6)
#   y<- c(3,5,7)
#   expect_type(weighted.vector.sum(x,y), "double")
#   #failure
#   expect_error(weighted.vector.sum(), "argument \"x\" is missing, with no default")
# })
#
#   # calc.ratio
# test_that("calc.ratio() tests", {
#   #success
#   a<- c(1)
#   b<- c(4)
#     #mean
#   expect_equal(calc.ratio(a,b,"mean"), 0.25)
#     #sum
#   expect_equal(calc.ratio(a,b,"sum"), 0.25)
#     #slope
#   expect_equal(calc.ratio(a,b,"slope"), 0.25)
#     #geometric.mean
#   expect_equal(calc.ratio(a,b,"geometric.mean"), 0.25)
#     #weighted.vector.sum
#   expect_equal(calc.ratio(a,b,"weighted.vector.sum"), 0.25)
#     #median
#   expect_equal(calc.ratio(a,b,"median"), 0.25)
#
#   #failure
#   expect_error(calc.ratio(), "the condition has length > 1")
#
# })
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
