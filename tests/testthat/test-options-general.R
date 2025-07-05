test_that("safety checks work", {
  # define_pkg_option
  expect_error(define_pkg_option(check_fn = 42), "must be a function")
  expect_snapshot(
    error = TRUE,
    define_pkg_option(default = 42L, check_fn = function(x) stop("issue"))
  )
  expect_snapshot(
    error = TRUE,
    define_pkg_option(default = 42L, check_fn = function(x) {
      return("not T/F")
    })
  )
  expect_snapshot(
    error = TRUE,
    define_pkg_option(default = 42L, check_fn = function(x) {
      return(FALSE)
    })
  )

  # get_pkg_option
  expect_error(get_pkg_option("dne", "pkg"), "not defined")

  # set_pkg_option
  expect_error(set_pkg_option("dne", "value", "pkg"), "not defined")

  # pkg_options
  # -- errors already tested in get/set_pkg_option
})

test_that("functionality works", {
  # define_pkg_option
  expect_equal(opt_a <- define_pkg_option(), list())
  expect_equal(opt_b <- define_pkg_option("default"), list(default = "default"))
  expect_equal(
    opt_c <- define_pkg_option(42, is_integerish),
    list(default = 42, check_fn_quo = quo(is_integerish))
  )
  .options <- list(a = opt_a, b = opt_b, c = opt_c)

  # set/get_pkg_option
  opts <- options()
  on.exit(options(opts))

  ## a (no default, no check)
  expect_equal(get_pkg_option("a", "pkg", .options), NULL)
  expect_silent(set_pkg_option("a", "test", "pkg", .options))
  expect_equal(getOption("pkg.a"), "test")
  expect_equal(get_pkg_option("a", "pkg", .options), "test")
  expect_silent(set_pkg_option("a", 42, "pkg", .options))
  expect_equal(get_pkg_option("a", "pkg", .options), 42)
  options(pkg.a = 2.7)
  expect_equal(get_pkg_option("a", "pkg", .options), 2.7)

  ## b (with default)
  expect_equal(get_pkg_option("b", "pkg", .options), "default")
  expect_silent(set_pkg_option("b", "test", "pkg", .options))
  expect_equal(get_pkg_option("b", "pkg", .options), "test")
  expect_silent(set_pkg_option("b", "default", "pkg", .options))
  expect_null(getOption("pkg.b"))
  expect_equal(get_pkg_option("b", "pkg", .options), "default")

  ## c (with check)
  expect_snapshot(error = TRUE, set_pkg_option("c", "42", "pkg", .options))
  options(pkg.c = "42")
  expect_snapshot(error = TRUE, get_pkg_option("c", "pkg", .options))
  expect_silent(set_pkg_option("c", 42, "pkg", .options))
  expect_equal(get_pkg_option("c", "pkg", .options), 42)

  # pkg_options
  options(pkg.a = NULL, pkg.b = NULL, pkg.c = NULL)
  old <- expect_visible(
    pkg_options("pkg", .options),
    list(a = NULL, b = "default", c = 42)
  )
  old2 <- expect_invisible(pkg_options("pkg", .options, a = "new", b = "hello"))
  expect_equal(old2, list(a = NULL, b = "default"))
  new <- expect_visible(pkg_options("pkg", .options, c = 1, "b"))
  expect_equal(new, list(c = 42, b = "hello"))
  new2 <- expect_visible(pkg_options("pkg", .options))
  expect_equal(new2, list(a = "new", b = "hello", c = 1))
  expect_invisible(pkg_options("pkg", .options, old))
  expect_equal(
    options("pkg.a", "pkg.b", "pkg.c"),
    list(pkg.a = NULL, pkg.b = NULL, pkg.c = NULL)
  )
  expect_equal(
    pkg_options("pkg", .options),
    list(a = NULL, b = "default", c = 42)
  )
})
