# Internal utility functions =============

## factorize_dataset() =============

test_that("factorize_dataset()", {
  # failure
  expect_error(
    factorize_dataset(),
    "argument \"dataset\" is missing, with no default"
  )

  df <- suppressMessages(
    orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox"))
  )

  expect_silent(factorize_dataset(df))
})

## group_if_exists() =============

test_that("group_if_exists()", {
  # failure
  expect_error(
    group_if_exists(),
    "argument \"dataset\" is missing, with no default"
  )

  expect_error(
    group_if_exists(tibble()),
    "argument \"cols\" is missing, with no default"
  )
})

## group_by_same_groups() =============

test_that("group_by_same_groups()", {
  # failure
  expect_error(
    group_by_same_groups(),
    "argument \"target_dataset\" is missing, with no default"
  )

  expect_error(
    group_by_same_groups(42),
    "no applicable method for 'group_by' applied to an object of class \"c('double', 'numeric')\"",
    fixed = TRUE
  )
})

## count_grouped_distinct() =============

test_that("count_grouped_distinct()", {
  # failure
  expect_error(
    count_grouped_distinct(),
    "argument \"dataset\" is missing, with no default"
  )

  expect_error(
    count_grouped_distinct(42),
    "no applicable method for 'select' applied to an object of class \"c('double', 'numeric')\"",
    fixed = TRUE
  )
})
