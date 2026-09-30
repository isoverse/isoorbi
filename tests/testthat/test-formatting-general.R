test_that("bytes_to_text()", {
  expect_equal(bytes_to_text(500), "500 bytes")
  expect_equal(bytes_to_text(1024^2), "1 Mb")
  expect_equal(bytes_to_text(NA), NA_character_)
  expect_equal(
    bytes_to_text(c(500, NA)),
    c("500 bytes", NA_character_)
  )
})

test_that("numbers_to_text()", {
  # metric prefixes, trimmed by default
  expect_equal(
    numbers_to_text(c(0, 1500, 0.0023, 2.5e6)),
    c("0", "1.5k", "2.3m", "2.5M")
  )
  # special values pass through as-is (NA stays NA_character_)
  expect_equal(numbers_to_text(c(NA, Inf, NaN)), c(NA, "Inf", "NaN"))
  # empty input
  expect_equal(numbers_to_text(numeric(0)), character(0))
  # non-numeric input errors
  expect_error(numbers_to_text("x"))
})

test_that("secs_to_text()", {
  expect_equal(secs_to_text(0.5), "500ms")
  expect_equal(secs_to_text(65), "1m 5s")
  expect_equal(secs_to_text(3661), "1h 1m 1s")
  expect_equal(secs_to_text(c(0.5, 65)), c("500ms", "1m 5s"))
  expect_error(secs_to_text("x"))
})
