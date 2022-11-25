# Tests: Common utility functions to clean and annotate data ------------------------------------

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("utils")


  # orbi_filter_weak_isotopocules
test_that("orbi_filter_weak_isotopocules() tests", {
  expect_error(orbi_filter_weak_isotopocules(), "no dataset supplied")

  expect_error(orbi_filter_weak_isotopocules(dataset = T), "dataset must be a data frame")


  df.true <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_error(orbi_filter_weak_isotopocules(dataset = df.true), "value for min_percent missing", fixed = TRUE)

  expect_error(orbi_filter_weak_isotopocules(dataset = df.true[,1:5]),
               "dataset must have at least 8 columns: 5",
               fixed = TRUE)

  expect_error(orbi_filter_weak_isotopocules(dataset = df.true[0,]),
               "dataset contains no rows",
               fixed = TRUE)

  expect_error(orbi_filter_weak_isotopocules(dataset = df.true, min_percent = T), "min_percent needs to be a number", fixed = TRUE)

  expect_error(orbi_filter_weak_isotopocules(dataset = df.true, min_percent = 100), "min_percent needs to be between 0 and 90", fixed = TRUE)

  expect_true(is.tbl(orbi_filter_weak_isotopocules(dataset = orbi_simplify_isox(df.true),
                                                   min_percent = 1
                                                   )))

  df.true2 <- df.true %>% mutate(block = as.factor("block1"), segment = as.factor("segment2"), injection = as.factor("injection3"))
  expect_true(is.tbl(orbi_filter_weak_isotopocules(dataset = df.true2,
                                                   min_percent = 1
  )))

  df.fail <- read.csv(file.path(base_dir, "test_files", "first10rows.csv")) %>% select(-scan.no)
  expect_error(orbi_filter_weak_isotopocules(dataset = df.fail, min_percent = 1))
})


### orbi_filter_satellite_peaks
test_that("orbi_filter_satellite_peaks() tests", {
  #succcess
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))
  expect_true(is.tbl(orbi_filter_satellite_peaks(orbi_simplify_isox(df))))
  #failure
  expect_error(orbi_filter_satellite_peaks(), "no dataset supplied")
  expect_error(orbi_filter_satellite_peaks(dataset = T), "dataset must be a data frame")


  expect_error(orbi_filter_satellite_peaks(dataset = df[, 1:5]), "dataset must have at least 8 columns: 5",
               fixed = TRUE)

  expect_error(orbi_filter_satellite_peaks(dataset = df[0,]), "dataset contains no rows",
               fixed = TRUE)


  df2 <- df %>% mutate(dummy = 1) %>% select(-scan.no)
  expect_error(orbi_filter_satellite_peaks(dataset = df2 ), "Missing required column(s): scan.no", fixed = TRUE)

})

### orbi_filter_scan_intensity
test_that("orbi_filter_scan_intensity() tests", {
  expect_error(orbi_filter_scan_intensity(), "no dataset supplied")

  expect_error(orbi_filter_scan_intensity(dataset = T), "dataset must be a data frame",
               fixed = TRUE)



  df <- read.csv(file.path(base_dir, "test_files", "first10rows.csv"))

  expect_error(orbi_filter_scan_intensity(dataset = df),
               "value for outlier_percent missing",
               fixed = TRUE)

  expect_error(orbi_filter_scan_intensity(dataset = df, outlier_percent = T),
               "outlier_percent needs to be a number",
               fixed = TRUE)

  expect_error(orbi_filter_scan_intensity(dataset = df, outlier_percent = 1000),
               "outlier_percentt needs to be between 0 and 10",
               fixed = TRUE)

  df.fail <- df[,1:5]
  expect_error(orbi_filter_scan_intensity(dataset = df.fail),
               "dataset must have at least 8 columns: 5",
               fixed = TRUE)


  df.fail2 <- df %>% mutate(dummy=1) %>% select(-scan.no)
  expect_error(orbi_filter_scan_intensity(dataset = df.fail2, outlier_percent = 1),
               "Missing required column(s): scan.no",
               fixed = TRUE)

  df.fail3 <- df[0,]
  expect_error(orbi_filter_scan_intensity(dataset = df.fail3),
               "dataset contains no rows",
               fixed = TRUE)

  expect_true(is.tbl(orbi_filter_scan_intensity(dataset = df, outlier_percent =0)))


  df.true <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")) %>% mutate(block = as.factor("block1"), segment = as.factor("segment2"), injection =as.factor("injection3"))

  expect_true(is.tbl(orbi_filter_scan_intensity(dataset = df.true, outlier_percent =10)))

})



### orbi_define_basepeak()
test_that("orbi_define_basepeak() tests", {
  expect_error(orbi_define_basepeak(), "no input for dataset supplied")
})

test_that("orbi_define_basepeak() tests", {
  expect_error(orbi_define_basepeak(dataset = T), "dataset must be a data frame")
})

test_that("orbi_define_basepeak() tests", {
  df <- read.csv(file.path(base_dir, "test_files", "first10rows.csv"))
  expect_error(orbi_define_basepeak(dataset = df), "no input for basepeak_def supplied")
})

test_that("orbi_define_basepeak() tests", {
  df <- read.csv(file.path(base_dir, "test_files", "first10rows.csv"), stringsAsFactors = T)

  expect_true(is.tbl(orbi_define_basepeak(dataset = df, basepeak_def = "M0")))

  expect_error(orbi_define_basepeak(dataset = df, basepeak_def = F), "denominator must be a basepeak_def vector")
})

test_that("orbi_define_basepeak() tests", {
  df <- read.csv(file.path(base_dir, "test_files", "first10rows.csv"))
  expect_error(orbi_define_basepeak(dataset = df,
                                    basepeak_def = c("M0", "123")),
               "only one basepeak_def can be assigned")
})

test_that("orbi_define_basepeak() tests", {
  df <- read.csv(file.path(base_dir, "test_files", "first10rows.csv"),
                 stringsAsFactors = T)
  expect_error(orbi_define_basepeak(dataset = df,
                                    basepeak_def = c("ABC123")),
               "basepeak_def is not found in data")

  df.fail <- df %>% select(-scan.no)
  expect_error(orbi_define_basepeak(dataset = df.fail,
                                    basepeak_def = "M0"),
               "Missing expected column(s): scan.no", fixed = TRUE)
})


# Improve this code by adding omit.na() or similar

# test_that("orbi_define_basepeak() tests", {
#   df <- read.csv(file.path(base_dir, "test_files", "first10rows.csv"),
#                  stringsAsFactors = T)
#
#   df.fail <- df %>% filter(isotopocule!="M0")
#   expect_error(orbi_define_basepeak(dataset = df.fail,
#                                     basepeak_def = "M0"))
# })
