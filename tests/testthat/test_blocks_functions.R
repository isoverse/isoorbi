
context("blocks functions")

test_that("orbi_define_blocks_for_dual_inlet() tests", {



})



test_that("orbi_adjust_block() tests", {

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

  # value checks
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
