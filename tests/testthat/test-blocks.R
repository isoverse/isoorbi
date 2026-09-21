test_that("orbi_define_blocks()", {
  # type checks
  expect_error(
    orbi_define_blocks(),
    "dataset.*must be.*aggregated.*or.*data frame"
  )

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    suppressMessages()

  expect_error(
    orbi_define_blocks(df),
    "block definition.*incomplete"
  )
  expect_error(
    orbi_define_blocks(
      df,
      start_time.min = 0,
      start_scan.no = 5
    ),
    "block definition.*incomplete"
  )

  expect_error(
    orbi_define_blocks(df, start_time.min = "a"),
    "start_time.min.*must be a number"
  )
  expect_error(
    orbi_define_blocks(df, end_time.min = "a"),
    "end_time.min.*must be a number"
  )
  expect_error(
    orbi_define_blocks(df, start_scan.no = "a"),
    "start_scan.no.*must be a whole number"
  )
  expect_error(
    orbi_define_blocks(df, end_scan.no = "a"),
    "end_scan.no.*must be a whole number"
  )

  expect_error(
    orbi_define_blocks(
      df,
      start_time.min = 0,
      start_scan.no = 5,
      end_time.min = 2,
      end_scan.no = 10
    ),
    "block definition.*can either be by time or by scan but not both"
  )

  # results checks
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(6, 4)),
    scan.no = 1:10,
    time.min = scan.no / 10
  )
  suppressMessages(
    orbi_define_blocks(
      test_data,
      start_time.min = 0.1,
      end_time.min = 0.9
    ) |>
      expect_message("added 1 block")
  )

  # block_name sets the block_name column
  with_name <- orbi_define_blocks(
    test_data,
    start_time.min = 0.1,
    end_time.min = 0.9,
    block_name = "my block"
  ) |>
    suppressMessages()
  expect_true("block_name" %in% names(with_name))
  expect_equal(setdiff(unique(with_name$block_name), NA), "my block")

  # several blocks at once, via vectors and via a blocks table
  # note: blocks are numbered per file, so each file gets its own blocks 1 and 2
  multi_data <- tibble(
    filename = rep(c("test1", "test2"), each = 10),
    scan.no = rep(1:10, 2),
    time.min = scan.no / 10
  )
  multi <- orbi_define_blocks(
    multi_data,
    start_scan.no = c(1L, 4L),
    end_scan.no = c(2L, 5L),
    block_name = c("a", "b")
  ) |>
    suppressMessages()
  expect_equal(sort(unique(multi$block)), c(0L, 1L, 2L))
  expect_setequal(setdiff(unique(multi$block_name), NA), c("a", "b"))
  # both blocks cover their scans in both files
  expect_equal(sum(multi$block == 1L), 4L)
  expect_equal(sum(multi$block == 2L), 4L)
  expect_equal(
    orbi_define_blocks(
      multi_data,
      blocks_table = tibble(
        start_scan.no = c(1L, 4L),
        end_scan.no = c(2L, 5L),
        block_name = c("a", "b")
      )
    ) |>
      suppressMessages(),
    multi
  )

  # blocks that fall outside the data warn (but are still added where they fit),
  # by scan number...
  orbi_define_blocks(test_data, start_scan.no = 100L, end_scan.no = 200L) |>
    suppressMessages() |>
    expect_warning("outside the data in 2 files.*covers scans 1 to 6")
  # test1 holds scans 1-6 and test2 scans 7-10, so this only fits the first file
  partial <- NULL
  expect_warning(
    partial <- orbi_define_blocks(
      test_data,
      start_scan.no = 1L,
      end_scan.no = 4L
    ) |>
      suppressMessages(),
    "outside the data in 1 file.*test2"
  )
  expect_equal(sum(partial$block == 1L), 4L)
  # ... and by time, which is treated the same way (the file's time range is named)
  orbi_define_blocks(test_data, start_time.min = 5, end_time.min = 6) |>
    suppressMessages() |>
    expect_warning("outside the data in 2 files.*covers 0.1 to 0.6 min")
  orbi_define_blocks(test_data, start_time.min = 0.001, end_time.min = 0.01) |>
    suppressMessages() |>
    expect_warning("outside the data in 2 files")
  # a block that misses only some of the files is still added to the others
  time_partial <- NULL
  expect_warning(
    time_partial <- orbi_define_blocks(
      test_data,
      start_time.min = 0.1,
      end_time.min = 0.5
    ) |>
      suppressMessages(),
    "outside the data in 1 file.*test2"
  )
  expect_equal(sum(time_partial$block == 1L), 4L)
  # a reversed time range covers no scans either
  tibble(filename = "test1", scan.no = 1:10, time.min = scan.no / 10) |>
    orbi_define_blocks(start_time.min = 0.9, end_time.min = 0.2) |>
    suppressMessages() |>
    expect_warning("outside the data")

  # the summary reports what each block actually covers, not what was requested
  expect_message(
    orbi_define_blocks(multi_data, start_time.min = 0, end_time.min = 99) |>
      expect_message("added 1 block"),
    "covers scans 1 to 10 \\(0.1 to 1 min\\) in 2 files"
  )
  # and says so when a block could not be added anywhere
  suppressWarnings(
    # note: each summary bullet is its own message, so mop up the remaining one
    suppressMessages(
      expect_message(
        orbi_define_blocks(
          multi_data,
          start_time.min = c(0.2, 5),
          end_time.min = c(0.5, 6),
          block_name = c("here", "gone")
        ) |>
          expect_message("added 1 of 2 blocks"),
        "block gone: not added, outside the data in 2 files"
      )
    )
  )

  # blocks_table checks
  orbi_define_blocks(test_data, blocks_table = 42) |>
    expect_error("must be a data frame")
  orbi_define_blocks(test_data, blocks_table = tibble()) |>
    expect_error("at least one row")
  orbi_define_blocks(test_data, blocks_table = tibble(foo = 1)) |>
    expect_error("none of the block definition columns")
  orbi_define_blocks(
    test_data,
    start_time.min = c(0.1, 0.2, 0.3),
    end_time.min = c(0.4, 0.5)
  ) |>
    expect_error("cannot be recycled")
})

test_that("internal find_intervals()", {
  # type checks
  expect_error(find_intervals(), "`total_time` must a single number")
  expect_error(find_intervals("4.2"), "`total_time` must a single number")
  expect_error(find_intervals(c(4.2, 4.2)), "`total_time` must a single number")
  expect_error(find_intervals(42.5), "`intervals` must be one or more numbers")
  expect_error(
    find_intervals(42.5, "42"),
    "`intervals` must be one or more numbers"
  )

  # results checks - single interval
  expect_equal(
    find_intervals(7.5, 2.5),
    tibble(
      interval = 1:3,
      idx = 1L,
      start = c(0, 2.5, 5),
      length = c(2.5, 2.5, 2.5),
      end = c(2.5, 5, 7.5)
    )
  )
  expect_equal(
    find_intervals(7.4, 2.5),
    tibble(
      interval = 1:3,
      idx = 1L,
      start = c(0, 2.5, 5),
      length = c(2.5, 2.5, 2.4),
      end = c(2.5, 5, 7.4)
    )
  )
  # results checks - double interval
  expect_equal(
    find_intervals(8.0, c(1.0, 2.5)),
    tibble(
      interval = 1:5,
      idx = c(1L, 2L, 1L, 2L, 1L),
      start = c(0, 1, 3.5, 4.5, 7),
      length = c(1, 2.5, 1, 2.5, 1),
      end = c(1, 3.5, 4.5, 7, 8)
    )
  )
  expect_equal(
    find_intervals(7.5, c(1.0, 2.5)),
    tibble(
      interval = 1:5,
      idx = c(1L, 2L, 1L, 2L, 1L),
      start = c(0, 1, 3.5, 4.5, 7),
      length = c(1, 2.5, 1, 2.5, 0.5),
      end = c(1, 3.5, 4.5, 7, 7.5)
    )
  )
  # results checks - triple
  expect_equal(
    find_intervals(7.5, c(1.0, 2.5, 2.5)),
    tibble(
      interval = 1:5,
      idx = c(1L, 2L, 3L, 1L, 2L),
      start = c(0, 1, 3.5, 6, 7),
      length = c(1, 2.5, 2.5, 1, 0.5),
      end = c(1, 3.5, 6, 7, 7.5)
    )
  )
})

test_that("find_blocks()", {
  # type checks
  expect_error(find_blocks(), "dataset.* must be a data frame or tibble")
  expect_error(find_blocks(42), "dataset.* must be a data frame or tibble")
  expect_error(find_blocks(mtcars), "columns.*are missing")
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0)),
    "ref_block_time.min.*must be a single positive number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), "42"),
    "ref_block_time.min.*must be a single positive number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), 0),
    "ref_block_time.min.*must be a single positive number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), c(42, 42)),
    "ref_block_time.min.*must be a single positive number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), 1, "42"),
    "sample_block_time.min.*must be a single positive number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), 1, 0),
    "sample_block_time.min.*must be a single positive number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), 1, c(42, 42)),
    "sample_block_time.min.*must be a single positive number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), 1, 1, "42"),
    "startup_time.min.*must be a single number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), 1, 1, -0.1),
    "startup_time.min.*must be a single number"
  )
  expect_error(
    find_blocks(tibble(filename = "1", time.min = 0), 1, 1, c(42, 42)),
    "startup_time.min.*must be a single number"
  )

  # results check
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(6, 4)),
    scan.no = 1:10,
    time.min = scan.no / 10
  )
  expect_true(is.data.frame(res1 <- test_data |> find_blocks(0.2)))
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
  expect_true(is.data.frame(res2 <- test_data |> find_blocks(0.2, 0.3, 0.2)))
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

test_that("orbi_define_block_for_flow_injection() is deprecated", {
  # note: the deprecations use always = TRUE, so they warn on every call rather
  # than once every 8 hours - hence no need to force the lifecycle verbosity here
  # (and calling the function twice below warns both times)
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(6, 4)),
    scan.no = 1:10,
    time.min = scan.no / 10
  )
  expected <- orbi_define_blocks(
    test_data,
    start_time.min = 0.1,
    end_time.min = 0.9,
    block_name = "my block"
  ) |>
    suppressMessages()

  # the function itself is deprecated
  expect_warning(
    renamed <- orbi_define_block_for_flow_injection(
      test_data,
      start_time.min = 0.1,
      end_time.min = 0.9,
      block_name = "my block"
    ) |>
      suppressMessages(),
    "orbi_define_block_for_flow_injection.*deprecated"
  )
  expect_equal(renamed, expected)

  # as is the sample_name argument, which still forwards to block_name
  expect_warning(
    expect_warning(
      old_arg <- orbi_define_block_for_flow_injection(
        test_data,
        start_time.min = 0.1,
        end_time.min = 0.9,
        sample_name = "my block"
      ) |>
        suppressMessages(),
      "sample_name.*deprecated"
    ),
    "orbi_define_block_for_flow_injection.*deprecated"
  )
  expect_equal(old_arg, expected)
})

test_that("orbi_define_blocks_for_dual_inlet()", {
  # type checks
  expect_error(
    orbi_define_blocks_for_dual_inlet(),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(42),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble()),
    "`ref_block_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), "42"),
    "`ref_block_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 0),
    "`ref_block_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), c(42, 42)),
    "`ref_block_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1),
    "`change_over_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, "42"),
    "`change_over_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 0),
    "`change_over_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, c(42, 42)),
    "`change_over_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, "42"),
    "`sample_block_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, 0),
    "`sample_block_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, c(42, 42)),
    "`sample_block_time.min` must be a single positive number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, startup_time.min = "42"),
    "`startup_time.min` must be a single number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, startup_time.min = -0.1),
    "`startup_time.min` must be a single number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(
      tibble(),
      1,
      1,
      startup_time.min = c(42, 42)
    ),
    "`startup_time.min` must be a single number"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, ref_block_name = 42),
    "`ref_block_name` must be a single string"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(
      tibble(),
      1,
      1,
      ref_block_name = c("ref", "ref")
    ),
    "`ref_block_name` must be a single string"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 1, sample_block_name = 42),
    "`sample_block_name` must be a single string"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(
      tibble(),
      1,
      1,
      sample_block_name = c("sam", "sam")
    ),
    "`sample_block_name` must be a single string"
  )
  expect_error(
    orbi_define_blocks_for_dual_inlet(tibble(), 1, 1),
    "columns.*are missing"
  )

  # results checks
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(6, 4)),
    scan.no = 1:10,
    time.min = scan.no / 10
  )
  expect_message(
    res1 <- orbi_define_blocks_for_dual_inlet(test_data, 0.3, 0.1),
    "identified 4 blocks.*in.*2 file"
  ) |>
    suppressMessages()
  expect_equal(
    res1,
    test_data |>
      dplyr::mutate(
        data_group = c(1L, 1L, 2L, 3L, 3L, 3L, 1:4),
        block = rep(1:4, c(2, 4, 2, 2)),
        block_name = rep(c("ref", "sam", "ref", "sam"), c(2, 4, 2, 2)),
        data_type = c(
          "data",
          "data",
          "changeover",
          "data",
          "data",
          "data",
          "changeover",
          "data",
          "changeover",
          "data"
        ) |>
          factor(),
        segment = NA_integer_
      )
  )
  expect_message(
    res2 <- orbi_define_blocks_for_dual_inlet(
      test_data,
      0.2,
      change_over_time.min = 0.05,
      sample_block_time.min = 0.5,
      startup_time.min = 0.2
    ),
    "identified 4 blocks.*in.*2 file"
  ) |>
    suppressMessages()
  expect_equal(
    res2,
    test_data |>
      dplyr::mutate(
        data_group = c(1L, 2L, 2L, 3L, 4L, 4L, 1L, 1L, 2L, 3L),
        block = rep(0:3, c(1, 2, 5, 2)),
        block_name = rep(c("ref", "sam", "ref"), c(3, 5, 2)),
        data_type = c(
          "startup",
          "data",
          "data",
          "changeover",
          "data",
          "data",
          "data",
          "data",
          "changeover",
          "data"
        ) |>
          factor(),
        segment = NA_integer_
      )
  )
})

test_that("orbi_adjust_block()", {
  # type checks
  expect_error(
    orbi_adjust_block(),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(
    orbi_adjust_block(42),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(orbi_adjust_block(tibble()), "`block` must be a single integer")
  expect_error(
    orbi_adjust_block(tibble(), "42"),
    "`block` must be a single integer"
  )
  expect_error(
    orbi_adjust_block(tibble(), "4.2"),
    "`block` must be a single integer"
  )
  expect_error(
    orbi_adjust_block(tibble(), c(42, 42)),
    "`block` must be a single integer"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, 42),
    "if set, `filename` must be a single string"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, c("file", "file")),
    "if set, `filename` must be a single string"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, "file", shift_start_time.min = "42"),
    "if set, `shift_start_time.min` must be a single number"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, "file", shift_end_time.min = "42"),
    "if set, `shift_end_time.min` must be a single number"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, "file", shift_start_scan.no = 4.2),
    "if set, `shift_start_scan.no` must be a single integer"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, "file", shift_end_scan.no = 4.2),
    "if set, `shift_end_scan.no` must be a single integer"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, "file", set_start_time.min = "42"),
    "if set, `set_start_time.min` must be a single number"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, "file", set_end_time.min = "42"),
    "if set, `set_end_time.min` must be a single number"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, "file", set_start_scan.no = 4.2),
    "if set, `set_start_scan.no` must be a single integer"
  )
  expect_error(
    orbi_adjust_block(tibble(), 42, "file", set_end_scan.no = 4.2),
    "if set, `set_end_scan.no` must be a single integer"
  )

  # argument value checks
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(5, 1)),
    scan.no = 1:6,
    time.min = (1:6) / 10,
    data_group = rep(1:3, each = 2),
    block = rep(1:2, each = 3),
    block_name = "name",
    data_type = "data",
    segment = rep(c(NA_integer_, 1L), c(4, 2))
  )
  expect_error(
    orbi_adjust_block(tibble(), 1),
    "does not seem to have any block definitions yet"
  )
  expect_error(
    orbi_adjust_block(test_data, 1),
    "has data from more than 1 file"
  )
  expect_error(
    orbi_adjust_block(test_data, 1, "dne"),
    "filename.*is not in this.*dataset"
  )
  expect_error(
    orbi_adjust_block(test_data, 3, "test1"),
    "block.*is not in this.*dataset"
  )
  expect_error(
    orbi_adjust_block(
      test_data,
      1,
      "test1",
      shift_start_time.min = 42,
      shift_start_scan.no = 42
    ),
    "only provide ONE.*to change the block start"
  )
  expect_error(
    orbi_adjust_block(
      test_data,
      1,
      "test1",
      set_end_time.min = 42,
      set_end_scan.no = 42
    ),
    "only provide ONE.*to change the block end"
  )
  expect_error(
    orbi_adjust_block(test_data, 1, "test1", set_start_scan.no = 42),
    "does not contain scan"
  )
  expect_error(
    orbi_adjust_block(test_data, 1, "test1", set_start_scan.no = 5),
    "invalid scan range.*requested.*block cannot end before it starts"
  )
  expect_error(
    orbi_adjust_block(test_data, 1, "test1", set_start_time.min = 1),
    "invalid start time"
  )
  expect_error(
    orbi_adjust_block(test_data, 1, "test1", set_end_time.min = -1),
    "invalid end time"
  )

  # results check
  expect_message(
    result0 <- orbi_adjust_block(test_data, 1, "test1"),
    "made no changes"
  )
  expect_equal(test_data, result0)

  expect_message(
    result1 <- orbi_adjust_block(test_data, 2, "test1", set_start_time.min = 0),
    "made the following.*block.*adjustments"
  ) |>
    suppressMessages()
  expect_equal(result1$block, rep(2, 6))
  expect_equal(result1$data_group, rep(c(1, 3), c(5, 1)))
  expect_equal(result1$data_type, test_data$data_type)
  expect_equal(result1$segment, rep(c(NA_integer_, 1L), c(5, 1)))

  # capture success messages and results
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot({
      result2 <- orbi_adjust_block(
        test_data,
        1,
        "test1",
        shift_start_scan.no = 1
      )
    })

    expect_equal(result2$block, rep(c(1, 2), c(3, 3)))
    expect_equal(result2$data_group, rep(c(1, 2, 3), c(1, 2, 3)))
    expect_equal(result2$data_type, rep(c("unused", "data"), c(1, 5)))
    expect_equal(result2$segment, rep(c(NA_integer_, 1L), c(5, 1)))

    expect_snapshot(
      result3 <- orbi_adjust_block(
        test_data,
        2,
        "test1",
        shift_start_time.min = -1
      )
    )
    expect_equal(result3$block, rep(2, 6))
    expect_equal(result3$data_group, rep(c(1, 3), c(5, 1)))
    expect_equal(result3$data_type, test_data$data_type)
    expect_equal(result3$segment, rep(c(NA_integer_, 1L), c(5, 1)))

    expect_snapshot(
      result4 <- orbi_adjust_block(
        test_data,
        1,
        "test1",
        shift_end_scan.no = 1
      )
    )
    expect_snapshot(
      result5 <- orbi_adjust_block(
        test_data,
        1,
        "test1",
        shift_end_time.min = 1
      )
    )
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))
})

test_that("orbi_segment_block()", {
  # type checks
  expect_error(
    orbi_segment_blocks(),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(
    orbi_segment_blocks(42),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(
    orbi_segment_blocks(tibble(), into_segments = "42"),
    "`into_segments` must be a single positive integer"
  )
  expect_error(
    orbi_segment_blocks(tibble(), into_segments = -42),
    "`into_segments` must be a single positive integer"
  )
  expect_error(
    orbi_segment_blocks(tibble(), into_segments = 4.2),
    "`into_segments` must be a single positive integer"
  )
  expect_error(
    orbi_segment_blocks(tibble(), into_segments = c(42, 42)),
    "`into_segments` must be a single positive integer"
  )
  expect_error(
    orbi_segment_blocks(tibble(), by_scans = "42"),
    "`by_scans` must be a single positive integer"
  )
  expect_error(
    orbi_segment_blocks(tibble(), by_scans = -42),
    "`by_scans` must be a single positive integer"
  )
  expect_error(
    orbi_segment_blocks(tibble(), by_scans = 4.2),
    "`by_scans` must be a single positive integer"
  )
  expect_error(
    orbi_segment_blocks(tibble(), by_scans = c(42, 42)),
    "`by_scans` must be a single positive integer"
  )
  expect_error(
    orbi_segment_blocks(tibble(), by_time_interval = "42"),
    "`by_time_interval` must be a single positive number"
  )
  expect_error(
    orbi_segment_blocks(tibble(), by_time_interval = -42),
    "`by_time_interval` must be a single positive number"
  )
  expect_error(
    orbi_segment_blocks(tibble(), by_time_interval = c(42, 42)),
    "`by_time_interval` must be a single positive number"
  )
  expect_error(
    orbi_segment_blocks(tibble()),
    "does not seem to have any block definitions yet"
  )
  empty_data <- tibble(
    filename = character(),
    scan.no = integer(),
    time.min = numeric(),
    block = integer(),
    block_name = character(),
    data_type = character()
  )
  expect_error(
    orbi_segment_blocks(empty_data),
    "set one of the 3 ways to segment"
  )
  expect_error(
    orbi_segment_blocks(empty_data, into_segments = 5, by_scans = 10),
    "only set ONE of the 3 ways to segment"
  )

  # results check
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(6, 4)) |> forcats::as_factor(),
    scan.no = 1:10,
    time.min = scan.no^2 / 10,
    block = rep(c(1L, 2L, 1L), c(4, 2, 4)),
    block_name = c("test"),
    data_type = rep(c("unused", "data"), c(2, 8))
  )

  # check messages and data
  test_that_cli("cli", configs = c("plain", "fancy"), {
    # approach 1
    expect_snapshot(res1 <- test_data |> orbi_segment_blocks(into_segments = 2))
    expect_equal(
      res1,
      test_data |>
        dplyr::mutate(
          data_group = c(1L, 1:5, 1L, 1L, 2L, 2L),
          segment = c(NA, NA, 1:2, 1:2, 1L, 1L, 2L, 2L)
        ) |>
        dplyr::relocate(data_group, .before = "block")
    )
    # approach 2
    expect_snapshot(res2 <- test_data |> orbi_segment_blocks(by_scans = 2))
    expect_equal(
      res2,
      test_data |>
        dplyr::mutate(
          data_group = rep(c(1:3, 1:2), each = 2),
          segment = rep(c(NA, 1L, 2L), c(2, 6, 2)),
        ) |>
        dplyr::relocate(data_group, .before = "block")
    )

    # approach 3
    expect_snapshot(
      res3 <- test_data |> orbi_segment_blocks(by_time_interval = 1.0)
    )
    expect_equal(
      res3,
      test_data |>
        dplyr::mutate(
          data_group = c(1L, 1L, 2L, 2L, 3L, 4L, 1:4),
          segment = c(NA, NA, 1L, 1L, 1:2, 1:2, 4, 6),
        ) |>
        dplyr::relocate(data_group, .before = "block")
    )
  })
}) |>
  withr::with_options(new = list(show_exec_times = FALSE))

test_that("orbi_get_blocks_info()", {
  # type checks
  expect_error(
    orbi_get_blocks_info(),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(
    orbi_get_blocks_info(42),
    "dataset.*must be.*aggregated.*or.*data frame"
  )

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    suppressMessages()
  df2 <- df |> mutate(dummy = 1) |> select(-scan.no)

  expect_error(
    orbi_get_blocks_info(df2),
    "column.*missing"
  )
  expect_message(
    orbi_get_blocks_info(df),
    "does not seem to have any block definitions yet",
  )
})

test_that("orbi_add_blocks_to_plot()", {
  # type checks
  expect_error(
    orbi_add_blocks_to_plot(),
    "plot.*has to be a ggplot"
  )
  expect_error(
    orbi_add_blocks_to_plot(42),
    "plot.*has to be a ggplot"
  )

  df <- orbi_read_isox(orbi_get_example_files("testfile_dual_inlet.isox")) |>
    orbi_simplify_isox() |>
    orbi_define_blocks_for_dual_inlet(
      ref_block_time.min = 0.5,
      change_over_time.min = 0.1
    ) |>
    suppressMessages()

  vdiffr::expect_doppelganger(
    "intensity plot with blocks",
    orbi_plot_raw_data(df, y = ions.incremental)
  )
})

test_that("find_scan_from_time()", {
  # type checks
  expect_error(
    find_scan_from_time(),
    "argument \"scans\" is missing, with no default"
  )
  expect_error(
    find_scan_from_time(scans = "c"),
    "no applicable method for 'filter' applied to an object of class \"character\""
  )
  expect_error(
    find_scan_from_time(scans = TRUE),
    "no applicable method for 'filter' applied to an object of class \"logical\""
  )
})

test_that("get_scan_row()", {
  # type checks
  expect_error(get_scan_row(), "argument \"scan\" is missing, with no default")
  expect_error(
    get_scan_row(scans = 42),
    "argument \"scan\" is missing, with no default"
  )
})
