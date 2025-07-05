# standard errors and functionality tests are already in test-options-general.R

test_that("options works", {
  # deprecated settings
  expect_warning(orbi_set_settings(), "deprecated")
  expect_warning(orbi_get_settings(), "deprecated")

  # errors
  expect_error(orbi_get_option(), "argument.*missing")

  # data types
  expect_equal(
    orbi_get_options("data") |> names(),
    c(
      "data_type_data",
      "data_type_startup",
      "data_type_changeover",
      "data_type_unused"
    )
  )

  # all default settings
  expect_snapshot(orbi_get_options())
})
