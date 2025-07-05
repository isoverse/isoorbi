# factor_in_order
test_that("factor_in_order() tests", {
  # text factor
  x <- c("b", "b", "d", "b", "d", "a", "f", "a", "a", "b", "c")
  expect_s3_class(factor_in_order(x), "factor")
  expect_equal(factor_in_order(x) |> levels(), c("b", "d", "a", "f", "c"))
  expect_equal(factor_in_order(x) |> as.character(), x)
  expect_equal(factor_in_order(factor(x)), factor_in_order(x))

  # number factor
  y <- c(4, 3, 1, 3, 5, 5, 2, 1)
  expect_s3_class(factor_in_order(y), "factor")
  expect_equal(factor_in_order(y) |> levels(), c("4", "3", "1", "5", "2"))
  expect_equal(factor_in_order(y) |> as.character(), as.character(y))
  expect_equal(factor_in_order(factor(y)), factor_in_order(y))
})
