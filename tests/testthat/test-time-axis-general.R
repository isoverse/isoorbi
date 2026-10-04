test_that("get_timebase() picks an appropriate unit", {
  # interval of 60 s -> minutes; 3600 -> hours; 1 -> seconds
  expect_equal(names(get_timebase(60, cutoff = 0.5)), "m")
  expect_equal(names(get_timebase(3600, cutoff = 0.5)), "h")
  expect_equal(names(get_timebase(1, cutoff = 0.5)), "s")
  # sub-second
  expect_equal(names(get_timebase(1e-3, cutoff = 0.5)), "ms")
  # extremely small falls back to the smallest unit
  expect_equal(names(get_timebase(1e-20, cutoff = 0.5)), "fs")
})

test_that("breaks_pretty_duration() returns sensible breaks", {
  brk <- breaks_pretty_duration(n = 5)
  expect_type(brk, "closure")
  b <- brk(c(0, 120))
  expect_true(0 %in% b && 120 %in% b)
  expect_true(all(diff(b) > 0))
})

test_that("labels_duration() formats durations", {
  expect_equal(labels_duration()(c(0, 60, 120)), c("0 min", "1 min", "2 min"))
  # hours
  expect_equal(labels_duration()(c(0, 3600)), c("0 hours", "1 hours"))
  # short format drops the space and abbreviates the unit
  expect_equal(
    labels_duration(short_format = TRUE)(c(0, 3600)),
    c("0hr", "1hr")
  )
  # sub-second units (all values below a second use bare ms/µs/... labels)
  expect_equal(labels_duration()(c(0, 0.001, 0.002)), c("0 ms", "1 ms", "2 ms"))
  # NA passes through
  expect_true(is.na(labels_duration()(c(0, 60, NA))[3]))
})

test_that("labels_duration() keeps m:s/h:m:s format for sub-second spacing", {
  # large times with sub-second break spacing must keep their m:s / h:m:s format
  # and gain fractional seconds, not collapse to a single rounded ms/µs number
  expect_equal(
    labels_duration()(c(180, 180.001, 180.002)),
    c("3:00.000 min", "3:00.001 min", "3:00.002 min")
  )
  # microsecond spacing -> 6 fractional-second digits
  expect_equal(
    labels_duration()(c(180, 180.000001, 180.000002)),
    c("3:00.000000 min", "3:00.000001 min", "3:00.000002 min")
  )
  # the number of fractional digits matches the break spacing
  expect_equal(
    labels_duration()(c(180, 180.0005, 180.001)),
    c("3:00.0000 min", "3:00.0005 min", "3:00.0010 min")
  )
  # hours are kept too (h:m:s.sss), including short format
  expect_equal(
    labels_duration()(c(3600, 3600.001, 3600.002)),
    c("1:00:00.000 hours", "1:00:00.001 hours", "1:00:00.002 hours")
  )
  expect_equal(
    labels_duration(short_format = TRUE)(c(180, 180.001)),
    c("3:00.000m", "3:00.001m")
  )
  # whole-second values with sub-second spacing show fractional seconds
  expect_equal(
    labels_duration()(c(5, 5.001, 5.002)),
    c("5.000 secs", "5.001 secs", "5.002 secs")
  )
})
