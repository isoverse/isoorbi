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
    expect_warning("outside the data in 2 files.*covers 6s to 36s")
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
    "covers scans 1 to 10 \\(6s to 1m\\) in 2 files"
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

  # an infinite end means until the end of each file
  uneven_data <- tibble(
    filename = rep(c("test1", "test2"), c(10, 6)),
    scan.no = c(1:10, 1:6),
    time.min = scan.no / 10
  )
  to_end_by_scan <- orbi_define_blocks(
    uneven_data,
    start_scan.no = 3L,
    end_scan.no = Inf
  ) |>
    suppressMessages()
  expect_equal(
    to_end_by_scan |> dplyr::filter(block == 1L) |> dplyr::count(filename),
    tibble(filename = c("test1", "test2"), n = c(8L, 4L))
  )
  to_end_by_time <- orbi_define_blocks(
    uneven_data,
    start_time.min = 0.25,
    end_time.min = Inf
  ) |>
    suppressMessages()
  expect_equal(
    to_end_by_time |> dplyr::filter(block == 1L) |> dplyr::count(filename),
    tibble(filename = c("test1", "test2"), n = c(8L, 4L))
  )
  # also in a blocks table (mixed with finite ends)
  expect_equal(
    orbi_define_blocks(
      uneven_data,
      blocks_table = tibble(start_scan.no = c(1L, 3L), end_scan.no = c(2, Inf))
    ) |>
      suppressMessages() |>
      dplyr::filter(block == 2L) |>
      dplyr::count(filename),
    tibble(filename = c("test1", "test2"), n = c(8L, 4L))
  )
  # the summary reports the actual end
  orbi_define_blocks(uneven_data, start_scan.no = 3L, end_scan.no = Inf) |>
    expect_message("added 1 block") |>
    expect_message("covers scans 3 to 10")
  # but only the end can be infinite
  orbi_define_blocks(uneven_data, start_scan.no = Inf, end_scan.no = Inf) |>
    expect_error("start_scan.no.*must be finite")
  orbi_define_blocks(uneven_data, start_time.min = -Inf, end_time.min = 1) |>
    expect_error("start_time.min.*must be finite")
  orbi_define_blocks(uneven_data, start_scan.no = 1L, end_scan.no = -Inf) |>
    expect_error("end_scan.no.*cannot be.*-Inf")

  # in_filename checks
  orbi_define_blocks(
    multi_data,
    start_scan.no = 1L,
    end_scan.no = 2L,
    in_filename = 1
  ) |>
    expect_error("in_filename.*must be text")
  orbi_define_blocks(
    multi_data,
    start_scan.no = 1L,
    end_scan.no = 2L,
    in_filename = c("test1", "nope")
  ) |>
    expect_error("in_filename.*nope.*is not in this.*dataset")

  # a block for a single file is only added there (and the others are not
  # reported as outside the data)
  expect_no_warning(
    only_test2 <- orbi_define_blocks(
      multi_data,
      start_scan.no = 1L,
      end_scan.no = 2L,
      in_filename = "test2"
    ) |>
      suppressMessages()
  )
  expect_equal(
    only_test2 |>
      dplyr::filter(block == 1L) |>
      dplyr::select(filename, scan.no),
    tibble(filename = "test2", scan.no = 1:2)
  )
  expect_true(all(only_test2$block[only_test2$filename == "test1"] == 0L))
  expect_message(
    orbi_define_blocks(
      multi_data,
      start_scan.no = 1L,
      end_scan.no = 2L,
      in_filename = "test2"
    ) |>
      expect_message("added 1 block to 1 file"),
    "block in test2: covers scans 1 to 2 \\(6s to 12s\\)\\s*$"
  )

  # in_filename is recycled like the other parameters, so the same block for
  # every file is the same as a block without in_filename
  expect_equal(
    orbi_define_blocks(
      multi_data,
      start_scan.no = 1L,
      end_scan.no = 2L,
      in_filename = c("test1", "test2")
    ) |>
      suppressMessages(),
    orbi_define_blocks(multi_data, start_scan.no = 1L, end_scan.no = 2L) |>
      suppressMessages()
  )
  # and different blocks for different files are each numbered per file
  per_file <- orbi_define_blocks(
    multi_data,
    start_scan.no = c(1L, 5L),
    end_scan.no = c(2L, 6L),
    in_filename = c("test1", "test2")
  ) |>
    suppressMessages()
  expect_equal(
    per_file |> dplyr::filter(block == 1L) |> dplyr::select(filename, scan.no),
    tibble(filename = rep(c("test1", "test2"), each = 2), scan.no = c(1:2, 5:6))
  )
  expect_equal(sort(unique(per_file$block)), c(0L, 1L))

  # via a blocks table, where NA means all files
  from_table <- orbi_define_blocks(
    multi_data,
    blocks_table = tibble(
      start_scan.no = c(1L, 4L),
      end_scan.no = c(2L, 5L),
      in_filename = c(NA, "test1")
    )
  ) |>
    suppressMessages()
  expect_equal(
    from_table |>
      dplyr::filter(block > 0L) |>
      dplyr::count(filename, block),
    tibble(
      filename = c("test1", "test1", "test2"),
      block = c(1L, 2L, 1L),
      n = 2L
    )
  )

  # a block outside the data of its file only warns about that file
  orbi_define_blocks(
    test_data,
    start_scan.no = 7L,
    end_scan.no = 8L,
    in_filename = "test1"
  ) |>
    suppressMessages() |>
    expect_warning("in test1.*outside the data in 1 file.*covers scans 1 to 6")

  # overlapping blocks are only a problem within the same file
  expect_no_error(
    orbi_define_blocks(
      multi_data,
      start_scan.no = 1L,
      end_scan.no = 5L,
      in_filename = c("test1", "test2")
    ) |>
      suppressMessages()
  )
  orbi_define_blocks(
    multi_data,
    start_scan.no = c(1L, 4L),
    end_scan.no = c(5L, 6L),
    in_filename = "test1"
  ) |>
    suppressMessages() |>
    expect_error("block in test1: scan 4 to 6.*overlaps")

  # works the same with aggregated data (using the filename from the file info)
  agg <- system.file("extdata", package = "isoorbi") |>
    orbi_find_raw(include_cache = TRUE) |>
    orbi_read_raw(show_progress = FALSE) |>
    orbi_aggregate_raw(show_progress = FALSE) |>
    suppressMessages()
  agg_block <- agg |>
    orbi_define_blocks(
      start_scan.no = 1L,
      end_scan.no = 5L,
      in_filename = "nitrate_test_10scans"
    ) |>
    suppressMessages()
  expect_equal(
    agg_block |>
      orbi_get_data(file_info = "filename", scans = c("scan.no", "block")) |>
      suppressMessages() |>
      dplyr::filter(block == 1L) |>
      dplyr::select(-"uidx"),
    tibble(filename = "nitrate_test_10scans", scan.no = 1:5, block = 1L),
    ignore_attr = "unused_columns"
  )
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

test_that("orbi_adjust_blocks()", {
  # type checks
  expect_error(
    orbi_adjust_blocks(),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(
    orbi_adjust_blocks(42),
    "dataset.*must be.*aggregated.*or.*data frame"
  )
  expect_error(orbi_adjust_blocks(tibble()), "block.*is required")
  expect_error(
    orbi_adjust_blocks(tibble(), "42"),
    "block.*must be a whole number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 4.2),
    "block.*must be a whole number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), NA_integer_),
    "block adjustment 1 is missing the.*block"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, 42),
    "in_filename.*must be text"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, "file", shift_start_time.min = "42"),
    "shift_start_time.min.*must be a number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, "file", shift_end_time.min = "42"),
    "shift_end_time.min.*must be a number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, "file", shift_start_scan.no = 4.2),
    "shift_start_scan.no.*must be a whole number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, "file", shift_end_scan.no = 4.2),
    "shift_end_scan.no.*must be a whole number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, "file", set_start_time.min = "42"),
    "set_start_time.min.*must be a number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, "file", set_end_time.min = "42"),
    "set_end_time.min.*must be a number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, "file", set_start_scan.no = 4.2),
    "set_start_scan.no.*must be a whole number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 42, "file", set_end_scan.no = 4.2),
    "set_end_scan.no.*must be a whole number"
  )
  expect_error(
    orbi_adjust_blocks(tibble(), 1:3, shift_start_scan.no = 1:2),
    "cannot be recycled"
  )
  expect_error(
    orbi_adjust_blocks(
      tibble(),
      1:2,
      shift_start_time.min = c(42, NA),
      shift_start_scan.no = c(42, NA)
    ),
    "only provide ONE.*to change the block start.*block adjustment 1\\)"
  )
  expect_error(
    orbi_adjust_blocks(
      tibble(),
      1,
      set_end_time.min = 42,
      set_end_scan.no = 42
    ),
    "only provide ONE.*to change the block end"
  )

  # blocks_table checks
  orbi_adjust_blocks(tibble(), blocks_table = 42) |>
    expect_error("must be a data frame")
  orbi_adjust_blocks(tibble(), blocks_table = tibble()) |>
    expect_error("at least one row")
  orbi_adjust_blocks(tibble(), blocks_table = tibble(foo = 1)) |>
    expect_error("requires a.*block.*column")

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
    orbi_adjust_blocks(tibble(), 1),
    "does not seem to have any block definitions yet"
  )
  expect_error(
    orbi_adjust_blocks(test_data, 1, "dne"),
    "in_filename.*dne.*is not in this.*dataset"
  )
  expect_error(
    orbi_adjust_blocks(test_data, 3, "test1"),
    "block.*3.*is not in file.*test1"
  )
  expect_error(
    orbi_adjust_blocks(test_data, 3),
    "block.*3.*is not in this.*dataset"
  )
  expect_error(
    orbi_adjust_blocks(test_data, 1, "test1", set_start_scan.no = 42),
    "does not contain scan"
  )
  expect_error(
    orbi_adjust_blocks(test_data, 1, "test1", set_start_scan.no = 5),
    "invalid scan range.*requested.*block cannot end before it starts"
  )
  expect_error(
    orbi_adjust_blocks(test_data, 1, "test1", set_start_time.min = 1),
    "invalid start time"
  )
  expect_error(
    orbi_adjust_blocks(test_data, 1, "test1", set_end_time.min = -1),
    "invalid end time"
  )

  # results check
  expect_message(
    result0 <- orbi_adjust_blocks(test_data, 1, "test1"),
    "made no changes"
  )
  expect_equal(test_data, result0)

  expect_message(
    result1 <- orbi_adjust_blocks(
      test_data,
      2,
      "test1",
      set_start_time.min = 0
    ),
    "adjusted 1 block in 1 file"
  ) |>
    suppressMessages()
  expect_equal(result1$block, rep(2, 6))
  expect_equal(result1$data_group, rep(c(1, 3), c(5, 1)))
  expect_equal(result1$data_type, test_data$data_type)
  expect_equal(result1$segment, rep(c(NA_integer_, 1L), c(5, 1)))

  # without in_filename, the block is adjusted in all files that have it
  # (and the others are mentioned)
  multi_file_data <- tibble(
    filename = rep(c("test1", "test2"), each = 6),
    scan.no = rep(1:6, 2),
    time.min = scan.no / 10,
    block = rep(rep(1:2, each = 3), 2),
    block_name = "name",
    data_type = "data"
  )
  expect_message(
    all_files <- orbi_adjust_blocks(
      multi_file_data,
      1,
      shift_start_scan.no = 1
    ),
    "adjusted 2 blocks in 2 files"
  ) |>
    suppressMessages()
  expect_equal(
    all_files$data_type,
    rep(rep(c("unused", "data"), c(1, 5)), 2)
  )
  orbi_adjust_blocks(
    multi_file_data |> dplyr::filter(!(filename == "test2" & block == 1L)),
    1,
    shift_start_scan.no = 1
  ) |>
    suppressMessages() |>
    expect_warning("block.*1.*is not in.*file.*test2.*not adjusted there")

  # several adjustments at once, via vectors and via a blocks table
  expect_message(
    several <- orbi_adjust_blocks(
      multi_file_data,
      block = c(1, 2),
      in_filename = "test1",
      shift_start_scan.no = c(1, NA),
      shift_end_scan.no = c(NA, -1)
    ),
    "adjusted 2 blocks in 1 file"
  ) |>
    suppressMessages()
  expect_equal(
    several$data_type,
    c("unused", rep("data", 4), "unused", rep("data", 6))
  )
  expect_equal(
    orbi_adjust_blocks(
      multi_file_data,
      blocks_table = tibble(
        block = c(1, 2),
        in_filename = "test1",
        shift_start_scan.no = c(1, NA),
        shift_end_scan.no = c(NA, -1)
      )
    ) |>
      suppressMessages(),
    several
  )

  # which other blocks are affected (block 0 = scans not in any block)
  gap_data <- tibble(
    filename = "test1",
    scan.no = 1:10,
    time.min = scan.no / 10,
    block = c(0L, 1L, 1L, 1L, 0L, 2L, 2L, 2L, 2L, 0L),
    block_name = "name",
    data_type = rep(
      c("unused", "data", "unused", "data", "unused"),
      c(1, 3, 1, 4, 1)
    )
  )
  adjust_messages <- function(...) {
    testthat::capture_messages(orbi_adjust_blocks(gap_data, ...)) |>
      paste(collapse = "")
  }
  # into the gap: no other block is affected
  into_gap <- adjust_messages(2, shift_start_scan.no = -1)
  expect_match(into_gap, "moved start from scan 6")
  expect_no_match(into_gap, "block 1|block 0|removed")
  # into part of block 1: block 1 is shortened
  into_block <- adjust_messages(2, set_start_scan.no = 3)
  expect_match(into_block, "moved the end of block 1 to the new start")
  expect_no_match(into_block, "removed")
  # over all of block 1: block 1 is gone
  over_block <- adjust_messages(2, set_start_scan.no = 2)
  expect_match(over_block, "removed block 1 entirely")
  expect_no_match(over_block, "moved the end of block 1")

  # capture success messages and results
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot({
      result2 <- orbi_adjust_blocks(
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
      result3 <- orbi_adjust_blocks(
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
      result4 <- orbi_adjust_blocks(
        test_data,
        1,
        "test1",
        shift_end_scan.no = 1
      )
    )
    expect_snapshot(
      result5 <- orbi_adjust_blocks(
        test_data,
        1,
        "test1",
        shift_end_time.min = 1
      )
    )
    expect_snapshot(
      result6 <- orbi_adjust_blocks(
        multi_file_data,
        block = c(1, 2),
        shift_start_scan.no = c(1, NA),
        shift_end_scan.no = c(NA, -1)
      )
    )
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))
})

test_that("orbi_adjust_block() is deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  test_data <- tibble(
    filename = rep(c("test1", "test2"), c(5, 1)),
    scan.no = 1:6,
    time.min = (1:6) / 10,
    block = rep(1:2, each = 3),
    block_name = "name",
    data_type = "data"
  )
  expect_warning(
    deprecated <- orbi_adjust_block(
      test_data,
      1,
      "test1",
      shift_start_scan.no = 1
    ) |>
      suppressMessages(),
    "orbi_adjust_block.*deprecated.*orbi_adjust_blocks"
  )
  expect_equal(
    deprecated,
    orbi_adjust_blocks(test_data, 1, "test1", shift_start_scan.no = 1) |>
      suppressMessages()
  )
  # still requires the filename if there is more than one file
  expect_error(
    orbi_adjust_block(test_data, 1) |> suppressWarnings(),
    "has data from more than 1 file"
  )
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

  # block labels in the legend: data blocks as "data" (default) or by name
  fill_labels <- function(plot) {
    ggplot2::ggplot_build(plot)$plot$scales$get_scales("fill")$get_limits()
  }
  expect_equal(
    fill_labels(orbi_plot_raw_data(df, y = ions.incremental)),
    "data"
  )
  expect_equal(
    fill_labels(
      orbi_plot_raw_data(df, y = ions.incremental, use_data_block_names = TRUE)
    ),
    c("ref", "sam")
  )
  # other blocks keep their data type
  expect_equal(
    fill_labels(
      orbi_plot_raw_data(df, y = ions.incremental, add_all_blocks = TRUE)
    ),
    c("changeover", "data")
  )
  expect_equal(
    fill_labels(
      orbi_plot_raw_data(
        df,
        y = ions.incremental,
        add_all_blocks = TRUE,
        use_data_block_names = TRUE
      )
    ),
    c("ref", "sam", "changeover")
  )
  # unnamed data blocks are still labeled as "data"
  expect_equal(
    df |>
      dplyr::mutate(block_name = NA_character_) |>
      orbi_plot_raw_data(y = ions.incremental, use_data_block_names = TRUE) |>
      fill_labels(),
    "data"
  )
  expect_error(
    orbi_plot_raw_data(df, y = ions.incremental) |>
      orbi_add_blocks_to_plot(use_data_block_names = "yes"),
    "use_data_block_names.*must be TRUE or FALSE"
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
