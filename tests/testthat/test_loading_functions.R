# Tests: Functions to load, pre-filter and simplify IsoX data

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("loading functions")

test_that("orbi_read_isox() tests", {

  # safety checks

  expect_error(orbi_read_isox(), "no file path supplied",
               fixed = TRUE)

  # test safety checks
  expect_error(orbi_read_isox(), "no file path supplied")

  # safety checks

  expect_error(orbi_read_isox(), "no file path supplied",
               fixed = TRUE)

  expect_error(orbi_read_isox(c("one", "two")), "can only read.*1")

  expect_error(orbi_read_isox("DNE"), "does not exist",
               fixed = TRUE)

  temp_file <- tempfile(fileext = ".wrong")

  cat("empty", file = temp_file) # create the temp file

  expect_error(orbi_read_isox(temp_file), "unrecognized",
               fixed = TRUE)

  unlink(temp_file) # destroy the temp file

  # corrupt files - missing columns
  expect_error(orbi_read_isox(file.path(base_dir, "test_files", "missing_column.isox")), "file format error")
  # corrupt files
  expect_error(orbi_read_isox(file.path(
    base_dir, "test_files", "missing_column.isox"
  )), "file format error",
  fixed = TRUE)

  # test reading a file

  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(df))

  expect_equal(
    names(df),
    c(
      "filename",
      "scan.no",
      "time.min",
      "compound",
      "isotopocule",
      "ions.incremental",
      "tic",
      "it.ms"
    )
  )

  expect_equal(nrow(df), 5184)

})

# orbi_simplify_isox
test_that("orbi_simplify_isox() tests", {

  # success
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(orbi_simplify_isox(df)))

  # test safety checks
  expect_error(orbi_simplify_isox(), "no dataset supplied")
  expect_error(orbi_simplify_isox(dataset = "string"), "dataset must be a data frame")
  expect_type(df, "list")
  dataset = subset(df, select = -c(filename))
  expect_error(orbi_simplify_isox(dataset), "dataset must have at least 8 columns: ")
  dataset = df[0,]
  expect_error(orbi_simplify_isox(dataset), "dataset contains no rows: ")
  # failure

  expect_error(orbi_simplify_isox(), "no dataset supplied",
               fixed = TRUE)

  expect_error(orbi_simplify_isox(dataset = as.matrix(df)), "dataset must be a data frame", fixed = TRUE)

  expect_error(orbi_simplify_isox(dataset = df[,1:5]),
               "dataset must have at least 8 columns: 5")

  expect_error(orbi_simplify_isox(dataset = df[0,]),
               "dataset contains no rows")

  df2 <- df %>% mutate(dummy = "1") %>% select(-scan.no)
  expect_error(orbi_simplify_isox(dataset = df2),
               "Missing required column(s): scan.no", fixed = TRUE)

})

# orbi_filter_isox()

test_that("orbi_filter_isox() tests",{

  # success
  df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi"))

  expect_true(is.tbl(orbi_filter_isox(df)))

  expect_true(is.tbl(orbi_filter_isox(df,
                                      filenames = "20220125_01",
                                      compounds = "NO3-",
                                      isotopocules = c("15N", "18O"),
                                      time_min = 0.5,
                                      time_max = 2)))

  # failure
  expect_error(orbi_filter_isox(), "no dataset supplied",
               fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = as.matrix(df)), "dataset must be a data frame",
               fixed = TRUE)

  expect_error(orbi_filter_isox(dataset = df[,1:5]),
               "dataset must have at least 8 columns: 5")

  expect_error(orbi_filter_isox(dataset = df[0,]),
               "dataset contains no rows")

  expect_error(orbi_filter_isox(dataset = df,
                                time_min = "A"),
               "time_min needs to be a number")

  expect_error(orbi_filter_isox(dataset = df,
                                time_max = "A"),
               "time_max needs to be a number")

  expect_error(orbi_filter_isox(dataset = df,
                                time_min = c(0.1, 0.2)),
               "time_min needs to be a single number")

  expect_error(orbi_filter_isox(dataset = df,
                                time_max = c(0.1, 0.2)),
               "time_max needs to be a single number")

  expect_error(orbi_filter_isox(dataset = df,
                                filenames = as.matrix(c(1, 0))),
               "filenames needs to be a vector of names")

  expect_error(orbi_filter_isox(dataset = df,
                                isotopocules = as.matrix(c(1, 0))),
               "isotopocules needs to be a vector of names")

  expect_error(orbi_filter_isox(dataset = df,
                                compounds = as.matrix(c(1, 0))),
               "compounds needs to be a vector of names")

})
