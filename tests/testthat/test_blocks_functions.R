
context("blocks functions")

test_that("test internal find_intervals()", {

  # type checks
  expect_error(find_intervals(), "`total_time` must a single number")
  expect_error(find_intervals("4.2"), "`total_time` must a single number")
  expect_error(find_intervals(c(4.2, 4.2)), "`total_time` must a single number")
  expect_error(find_intervals(42.5), "`intervals` must be one or more numbers")
  expect_error(find_intervals(42.5, "42"), "`intervals` must be one or more numbers")

  # results checks - single interval
  expect_equal(
    find_intervals(7.5, 2.5),
    tibble(
      interval = 1:3, idx = 1L,
      start = c(0, 2.5, 5), length = c(2.5, 2.5, 2.5), end = c(2.5, 5, 7.5)
    )
  )
  expect_equal(
    find_intervals(7.4, 2.5),
    tibble(
      interval = 1:3, idx = 1L,
      start = c(0, 2.5, 5), length = c(2.5, 2.5, 2.4), end = c(2.5, 5, 7.4)
    )
  )
  # results checks - double interval
  expect_equal(
    find_intervals(8.0, c(1.0, 2.5)),
    tibble(
      interval = 1:5, idx = c(1L, 2L, 1L, 2L, 1L),
      start = c(0, 1, 3.5, 4.5, 7), length = c(1, 2.5, 1, 2.5, 1), end = c(1, 3.5, 4.5, 7, 8)
    )
  )
  expect_equal(
    find_intervals(7.5, c(1.0, 2.5)),
    tibble(
      interval = 1:5, idx = c(1L, 2L, 1L, 2L, 1L),
      start = c(0, 1, 3.5, 4.5, 7), length = c(1, 2.5, 1, 2.5, 0.5), end = c(1, 3.5, 4.5, 7, 7.5)
    )
  )
  # results checks - triple
  expect_equal(
    find_intervals(7.5, c(1.0, 2.5, 2.5)),
    tibble(
      interval = 1:5, idx = c(1L, 2L, 3L, 1L, 2L),
      start = c(0, 1, 3.5, 6, 7), length = c(1, 2.5, 2.5, 1, 0.5), end = c(1, 3.5, 6, 7, 7.5)
    )
  )

})

test_that("test find_blocks()", {

  # type checks
  expect_error(find_blocks(), "`dataset` must be a data frame or tibble")
  expect_error(find_blocks(42), "`dataset` must be a data frame or tibble")
  expect_error(find_blocks(tibble()), "`ref_block_time.min` must be a single positive number")
  expect_error(find_blocks(tibble(), "42"), "`ref_block_time.min` must be a single positive number")
  expect_error(find_blocks(tibble(), 0), "`ref_block_time.min` must be a single positive number")
  expect_error(find_blocks(tibble(), c(42, 42)), "`ref_block_time.min` must be a single positive number")
  expect_error(find_blocks(tibble(), 1, "42"), "`sample_block_time.min` must be a single positive number")
  expect_error(find_blocks(tibble(), 1, 0), "`sample_block_time.min` must be a single positive number")
  expect_error(find_blocks(tibble(), 1, c(42, 42)), "`sample_block_time.min` must be a single positive number")
  expect_error(find_blocks(tibble(), 1, 1, "42"), "`startup_time.min` must be a single number")
  expect_error(find_blocks(tibble(), 1, 1, -0.1), "`startup_time.min` must be a single number")
  expect_error(find_blocks(tibble(), 1, 1, c(42, 42)), "`startup_time.min` must be a single number")
  expect_error(find_blocks(tibble(), 1, 1, 1), "`dataset` is missing the column")

  # results check
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(6, 4)),
    scan.no = 1:10, time.min = scan.no/10
  )
  expect_is(res1 <- test_data |> find_blocks(0.2), "data.frame")
  expect_equal(
    res1,
    tibble(
      filename = rep(c("test1", "test2"), c(3, 2)),
      min_time.min = rep(c(0.1, 0.7), c(3, 2)),
      max_time.min = rep(c(0.6, 1), c(3, 2)),
      block = c(1:3, 4:5),
      idx = c(1L, 2L, 1L, 2L, 1L),
      start = c(0, 0.2, 0.4, 0.6, 0.8),
      length = 0.2,
      end = .data$start + .data$length,
      last = c(FALSE, FALSE, TRUE, FALSE, TRUE)
    )
  )
  expect_is(res2 <- test_data |> find_blocks(0.2, 0.3, 0.2), "data.frame")
  expect_equal(
    res2,
    tibble(
      filename = rep(c("test1", "test2"), c(3, 2)),
      min_time.min = rep(c(0.1, 0.7), c(3, 2)),
      max_time.min = rep(c(0.6, 1), c(3, 2)),
      block = c(0:2, 3:4),
      idx = c(0L, 1L, 2L, 1L, 2L),
      start = c(0, 0.2, 0.4, 0.7, 0.9),
      length = c(0.2, 0.2, 0.2, 0.2, 0.1),
      end = .data$start + .data$length,
      last = c(FALSE, FALSE, TRUE, FALSE, TRUE)
    )
  )

})

test_that("test orbi_define_blocks_for_dual_inlet()", {

  # type checks
  expect_error(orbi_define_blocks_for_dual_inlet(), "`dataset` must be a data frame or tibble")
  expect_error(orbi_define_blocks_for_dual_inlet(42), "`dataset` must be a data frame or tibble")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble()), "`ref_block_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), "42"), "`ref_block_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 0), "`ref_block_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), c(42, 42)), "`ref_block_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1), "`change_over_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, "42"), "`change_over_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 0), "`change_over_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, c(42, 42)), "`change_over_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, "42"), "`sample_block_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, 0), "`sample_block_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, c(42, 42)), "`sample_block_time.min` must be a single positive number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, startup_time.min = "42"), "`startup_time.min` must be a single number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, startup_time.min = -0.1), "`startup_time.min` must be a single number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, startup_time.min = c(42, 42)), "`startup_time.min` must be a single number")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, ref_block_name = 42), "`ref_block_name` must be a single string")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, ref_block_name = c("ref", "ref")), "`ref_block_name` must be a single string")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, sample_block_name = 42), "`sample_block_name` must be a single string")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, sample_block_name = c("sam", "sam")), "`sample_block_name` must be a single string")
  expect_error(orbi_define_blocks_for_dual_inlet(tibble(), 1, 1), "`dataset` is missing the column")

  # results checks
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(6, 4)),
    scan.no = 1:10, time.min = scan.no/10
  )
  expect_message(res1 <- orbi_define_blocks_for_dual_inlet(test_data, 0.3, 0.1), "identified 4 blocks.*in.*2 file")
  expect_equal(
    res1,
    test_data |> dplyr::mutate(
      data_group = c(1L, 1L, 2L, 3L, 3L, 3L, 1:4),
      block = rep(1:4, c(2, 4, 2, 2)),
      sample_name = rep(c("ref", "sam", "ref", "sam"), c(2, 4, 2, 2)),
      data_type = c("data", "data", "changeover", "data", "data", "data", "changeover", "data", "changeover", "data") |>
        factor(),
      segment = NA_integer_
    )
  )
  expect_message(
    res2 <- orbi_define_blocks_for_dual_inlet(test_data, 0.2, change_over_time.min = 0.05,
                                              sample_block_time.min = 0.5, startup_time.min = 0.2),
    "identified 4 blocks.*in.*2 file")
  expect_equal(
    res2,
    test_data |> dplyr::mutate(
      data_group = c(1L, 2L, 2L, 3L, 4L, 4L, 1L, 1L, 2L, 3L),
      block = rep(0:3, c(1, 2, 5, 2)),
      sample_name = rep(c("ref", "sam", "ref"), c(3, 5, 2)),
      data_type = c("startup", "data", "data", "changeover", "data", "data", "data", "data", "changeover", "data") |>
        factor(),
      segment = NA_integer_
    )
  )
})

test_that("test orbi_adjust_block()", {

  # type checks
  expect_error(orbi_adjust_block(), "`dataset` must be a data frame or tibble")
  expect_error(orbi_adjust_block(42), "`dataset` must be a data frame or tibble")
  expect_error(orbi_adjust_block(tibble()), "`block` must be a single integer")
  expect_error(orbi_adjust_block(tibble(), "42"), "`block` must be a single integer")
  expect_error(orbi_adjust_block(tibble(), "4.2"), "`block` must be a single integer")
  expect_error(orbi_adjust_block(tibble(), c(42, 42)), "`block` must be a single integer")
  expect_error(orbi_adjust_block(tibble(), 42, 42), "if set, `filename` must be a single string")
  expect_error(orbi_adjust_block(tibble(), 42, c("file", "file")), "if set, `filename` must be a single string")
  expect_error(orbi_adjust_block(tibble(), 42, "file", shift_start_time.min = "42"), "if set, `shift_start_time.min` must be a single number")
  expect_error(orbi_adjust_block(tibble(), 42, "file", shift_end_time.min = "42"), "if set, `shift_end_time.min` must be a single number")
  expect_error(orbi_adjust_block(tibble(), 42, "file", shift_start_scan.no = 4.2), "if set, `shift_start_scan.no` must be a single integer")
  expect_error(orbi_adjust_block(tibble(), 42, "file", shift_end_scan.no = 4.2), "if set, `shift_end_scan.no` must be a single integer")
  expect_error(orbi_adjust_block(tibble(), 42, "file", set_start_time.min = "42"), "if set, `set_start_time.min` must be a single number")
  expect_error(orbi_adjust_block(tibble(), 42, "file", set_end_time.min = "42"), "if set, `set_end_time.min` must be a single number")
  expect_error(orbi_adjust_block(tibble(), 42, "file", set_start_scan.no = 4.2), "if set, `set_start_scan.no` must be a single integer")
  expect_error(orbi_adjust_block(tibble(), 42, "file", set_end_scan.no = 4.2), "if set, `set_end_scan.no` must be a single integer")

  # argument value checks
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(5, 1)),
    scan.no = 1:6, time.min = (1:6)/10,
    data_group = rep(1:3, each = 2), block = rep(1:2, each = 3),
    sample_name = "name", data_type = "data", segment = rep(c(NA_integer_, 1L), c(4, 2))
  )
  expect_error(orbi_adjust_block(tibble(), 1), "`dataset` is missing the column")
  expect_error(orbi_adjust_block(test_data, 1), "`dataset` has data from more than 1 file")
  expect_error(orbi_adjust_block(test_data, 1, "dne"), "`filename` is not in this `dataset`")
  expect_error(orbi_adjust_block(test_data, 3, "test1"), "`block` is not in this `dataset`")
  expect_error(orbi_adjust_block(test_data, 1, "test1", shift_start_time.min = 42, shift_start_scan.no = 42), "only provide ONE.*to change the block start")
  expect_error(orbi_adjust_block(test_data, 1, "test1", set_end_time.min = 42, set_end_scan.no = 42), "only provide ONE.*to change the block end")
  expect_error(orbi_adjust_block(test_data, 1, "test1", set_start_scan.no = 42), "does not contain scan")
  expect_error(orbi_adjust_block(test_data, 1, "test1", set_start_scan.no = 5), "invalid scan range.*requested.*block cannot end before it starts")
  expect_error(orbi_adjust_block(test_data, 1, "test1", set_start_time.min = 1), "invalid start time")
  expect_error(orbi_adjust_block(test_data, 1, "test1", set_end_time.min = -1), "invalid end time")

  # results check
  expect_message(result0 <- orbi_adjust_block(test_data, 1, "test1"), "not making any changes")
  expect_equal(test_data, result0)

  expect_message(result1 <- orbi_adjust_block(test_data, 2, "test1", set_start_time.min = 0), "is making the following block adjustments")
  expect_equal(result1$block, rep(2, 6))
  expect_equal(result1$data_group, rep(c(1, 3), c(5, 1)))
  expect_equal(result1$data_type, test_data$data_type)
  expect_equal(result1$segment, rep(c(NA_integer_, 1L), c(5, 1)))

  expect_message(result2 <- orbi_adjust_block(test_data, 1, "test1", shift_start_scan.no = 1), "moving block 1 start from scan 1.*to 2")
  expect_equal(result2$block, rep(c(1, 2), c(3, 3)))
  expect_equal(result2$data_group, rep(c(1, 2, 3), c(1, 2, 3)))
  expect_equal(result2$data_type, rep(c("unused", "data"), c(1, 5)))
  expect_equal(result2$segment, rep(c(NA_integer_, 1L), c(5, 1)))

  expect_message(result3 <- orbi_adjust_block(test_data, 2, "test1", shift_start_time.min = -1), "moving block 1 end to the new start of block 2")
  expect_equal(result3$block, rep(2, 6))
  expect_equal(result3$data_group, rep(c(1, 3), c(5, 1)))
  expect_equal(result3$data_type, test_data$data_type)
  expect_equal(result3$segment, rep(c(NA_integer_, 1L), c(5, 1)))
})

test_that("test orbi_segment_block()", {

  # type checks
  expect_error(orbi_segment_blocks(), "`dataset` must be a data frame or tibble")
  expect_error(orbi_segment_blocks(42), "`dataset` must be a data frame or tibble")
  expect_error(orbi_segment_blocks(tibble(), into_segments = "42"), "`into_segments` must be a single positive integer")
  expect_error(orbi_segment_blocks(tibble(), into_segments = -42), "`into_segments` must be a single positive integer")
  expect_error(orbi_segment_blocks(tibble(), into_segments = 4.2), "`into_segments` must be a single positive integer")
  expect_error(orbi_segment_blocks(tibble(), into_segments = c(42, 42)), "`into_segments` must be a single positive integer")
  expect_error(orbi_segment_blocks(tibble(), by_scans = "42"), "`by_scans` must be a single positive integer")
  expect_error(orbi_segment_blocks(tibble(), by_scans = -42), "`by_scans` must be a single positive integer")
  expect_error(orbi_segment_blocks(tibble(), by_scans = 4.2), "`by_scans` must be a single positive integer")
  expect_error(orbi_segment_blocks(tibble(), by_scans = c(42, 42)), "`by_scans` must be a single positive integer")
  expect_error(orbi_segment_blocks(tibble(), by_time_interval = "42"), "`by_time_interval` must be a single positive number")
  expect_error(orbi_segment_blocks(tibble(), by_time_interval = -42), "`by_time_interval` must be a single positive number")
  expect_error(orbi_segment_blocks(tibble(), by_time_interval = c(42, 42)), "`by_time_interval` must be a single positive number")
  expect_error(orbi_segment_blocks(tibble()), "`dataset` is missing the column")
  empty_data <- tibble(filename = character(), scan.no = integer(), time.min = numeric(), block = integer(), sample_name = character(), data_type = character())
  expect_error(orbi_segment_blocks(empty_data), "set one of the 3 ways to segment")
  expect_error(orbi_segment_blocks(empty_data, into_segments = 5, by_scans = 10), "only set ONE of the 3 ways to segment")

  # results check
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(6, 4)),
    scan.no = 1:10, time.min = scan.no^2/10,
    block = rep(c(1L, 2L, 1L), c(4, 2, 4)),
    sample_name = c("test"),
    data_type = rep(c("unused", "data"), c(2, 8))
  )

  expect_message(res1 <- test_data |> orbi_segment_blocks(into_segments = 2), "segmented 3 data blocks.*2 segments")
  expect_equal(
    res1,
    test_data |> dplyr::mutate(
      data_group = c(1L, 1:5, 1L, 1L, 2L, 2L),
      segment = c(NA, NA, 1:2, 1:2, 1L, 1L, 2L, 2L)
    ) |> dplyr::relocate(data_group, .before = "block")
  )

  expect_message(res2 <- test_data |> orbi_segment_blocks(by_scans = 2), "segmented 3 data blocks.*2 scans")
  expect_equal(
    res2,
    test_data |> dplyr::mutate(
      data_group = rep(c(1:3, 1:2), each = 2),
      segment = rep(c(NA, 1L, 2L), c(2, 6, 2)),
    ) |> dplyr::relocate(data_group, .before = "block")
  )

  expect_message(res3 <- test_data |> orbi_segment_blocks(by_time_interval = 1.0), "segmented 3 data blocks.*2.3 segments")
  expect_equal(
    res3,
    test_data |> dplyr::mutate(
      data_group = c(1L, 1L, 2L, 2L, 3L, 4L, 1:4),
      segment = c(NA, NA, 1L, 1L, 1:2, 1:2, 4, 6),
    ) |> dplyr::relocate(data_group, .before = "block")
  )

})

test_that("test orbi_get_blocks_info()", {

  # type checks
  expect_error(orbi_get_blocks_info(), "`dataset` must be a data frame or tibble")
  expect_error(orbi_get_blocks_info(42), "`dataset` must be a data frame or tibble")

})

test_that("test find_scan_from_time()", {

  # type checks
  expect_error(find_scan_from_time(), "argument \"scans\" is missing, with no default")
  expect_error(find_scan_from_time(scans = "c"), "no applicable method for 'filter' applied to an object of class \"character\"")
  expect_error(find_scan_from_time(scans = TRUE), "no applicable method for 'filter' applied to an object of class \"logical\"")

})

test_that("test get_scan_row()", {

  # type checks
  expect_error(get_scan_row(), "argument \"scan\" is missing, with no default")
  expect_error(get_scan_row(scans = 42), "argument \"scan\" is missing, with no default")

})

