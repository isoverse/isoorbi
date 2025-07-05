# functions for generic package options with type checking
# implement by writing a package specific get_<pkg>_option function that
# calls get_pkg_option and a <pkg>_options function that calls pkg_options
# in addition to defining a static list or get_package_options() function that
# returns a list e.g. `list(option_a = define_pkg_option(), option_b = ...)`
# requires rlang and cli imports

# define package options, checks that check_fn is indeed a
# function (if provided) and the default value is valid (if provided)
# @param default if this is an empty vector and check_fn is e.g. is_character,
# use `character(0)` because `c()` will (rightfully) not pass the check. Also,
# use is_integerish instead of the is_integer when checking for integer options
define_pkg_option <- function(default = NULL, check_fn = NULL) {
  check_fn_quo <- enquo(check_fn)
  check_fn <- eval_tidy(check_fn_quo)
  if (!is.null(check_fn)) {
    if (!is_function(check_fn)) {
      cli_abort(
        "{.var check_fn} must be a function but is {.obj_type_friendly {check_fn}}"
      )
    }
    if (!is.null(default)) {
      check_option_value("", default, check_fn_quo)
    }
  }
  # return option info
  option <- list()
  if (!is.null(default)) {
    option$default <- default
  }
  if (!is.null(check_fn)) {
    option$check_fn_quo <- check_fn_quo
  }

  return(option)
}

# get package option value (akin to base::getOption)
get_pkg_option <- function(
  option,
  pkg,
  pkg_options = list(),
  call = caller_env()
) {
  if (!option %in% names(pkg_options)) {
    cli::cli_abort(
      "option {.emph {option}} is not defined for
                   the {.pkg {pkg}} package",
      call = call
    )
  }
  value <- sprintf("%s.%s", pkg, option) |> getOption()

  # check in case user used base::options to set this option directly
  if (!is.null(value) && !is.null(pkg_options[[option]]$check_fn_quo)) {
    check_option_value(
      option,
      value,
      pkg_options[[option]]$check_fn_quo,
      call = call
    )
  }

  # default value
  if (is.null(value)) {
    value <- pkg_options[[option]]$default
  }
  return(value)
}

# set package option value
set_pkg_option <- function(
  option,
  value,
  pkg,
  pkg_options = list(),
  call = caller_env()
) {
  if (!option %in% names(pkg_options)) {
    cli::cli_abort(
      "option {.emph {option}} is not defined for
                   the {.pkg {pkg}} package",
      call = call
    )
  }

  # check if new value is default value
  if (!is.null(value) && identical(value, pkg_options[[option]]$default)) {
    value <- NULL
  } # reset back to default value

  # check to make sure new value is appropriate
  if (!is.null(value) && !is.null(pkg_options[[option]]$check_fn_quo)) {
    check_option_value(
      option,
      value,
      pkg_options[[option]]$check_fn_quo,
      call = call
    )
  }

  # set new value
  list(value) |>
    set_names(sprintf("%s.%s", pkg, option)) |>
    options()
}

# package options (akin to base::options)
pkg_options <- function(pkg, pkg_options = list(), ..., call = caller_env()) {
  dots <- dots_list(...)
  if (length(dots) == 0) {
    options <- names(pkg_options) # get all
    new_values_idx <- rep(FALSE, length(options))
  } else {
    if (length(dots) == 1L && is_bare_list(dots[[1]])) {
      dots <- dots[[1]]
    }
    new_values_idx <- nchar(names(dots)) > 0L
    names(dots)[!new_values_idx] <- sapply(dots[!new_values_idx], identity)
    options <- names(dots)
  }
  # retrieve original values
  old_values <- sapply(
    options,
    get_pkg_option,
    simplify = FALSE,
    pkg = pkg,
    pkg_options = pkg_options,
    call = call
  )

  # set new values
  if (any(new_values_idx)) {
    sapply(which(new_values_idx), function(i) {
      set_pkg_option(options[i], dots[[i]], pkg, pkg_options, call = call)
    })
  }

  # return invisible if all new values were set (same behavior as base::options)
  if (all(new_values_idx)) {
    return(invisible(old_values))
  }
  return(old_values)
}

# check if option value is valid
check_option_value <- function(
  option,
  value,
  check_fn_quo,
  call = caller_env()
) {
  # value quo and actual function
  value_quo <- enquo(value)
  check_fn <- eval_tidy(check_fn_quo)
  # try to run check
  check <-
    try_fetch(
      check_fn(eval_tidy(value_quo)),
      error = function(cnd) {
        if (is_call(cnd$call, "check_fn")) {
          cnd$call[[1]] <- check_fn_quo
        }
        cli_abort(
          "there was a problem while checking if {as_label(value_quo)}
          is valid for option {.emph {option}}",
          parent = cnd,
          call = call
        )
      }
    )
  # make sure it returned TRUE or FALSE
  if (!is_true(check) && !is_false(check)) {
    cli_abort(
      "option {.emph {option}} check function {.code {as_label(check_fn_quo)}}
      returned {.obj_type_friendly {check}} instead of TRUE/FALSE when checking
      {.var {as_label(value_quo)}}",
      call = call
    )
  }
  # see if it's valid / true
  if (!is_true(check)) {
    cli_abort(
      c(
        "invalid value for option {.emph {option}}",
        "i" = "{.var {as_label(value_quo)}} is {.obj_type_friendly {value}}",
        "x" = "the check function {.code {as_label(check_fn_quo)}} returned FALSE for this value"
      ),
      call = call
    )
  }
}
