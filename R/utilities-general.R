# general utility functions (any package) =======

# check function argument for condition (instead of stopifnot) for more informative error messages
check_arg <- function(
  x,
  condition,
  msg,
  arg = caller_arg(x),
  call = caller_env(),
  include_type = TRUE
) {
  if (!condition) {
    if (is_missing(maybe_missing(x))) {
      type = if (include_type) ", not missing" else ""
      cli_abort("argument {.field {arg}} {msg}{type}", call = call)
    } else {
      type <- if (include_type) {
        format_inline(", not {.obj_type_friendly {x}}")
      } else {
        ""
      }
      cli_abort("argument {.field {arg}} {msg}{type}", call = call)
    }
  }
}

# check tibble for being a tibble and required columns if there are any
check_tibble <- function(
  df,
  req_cols = c(),
  arg = caller_arg(df),
  call = caller_env()
) {
  check_arg(
    df,
    !missing(df) && is.data.frame(df),
    "must be a data frame or tibble",
    arg = arg,
    call = call
  )
  if (length(missing <- setdiff(req_cols, names(df))) > 0) {
    cli_abort(
      c(
        "{qty(missing)} column{?s} {.field {missing}} {?is/are} missing from {.field {arg}}",
        "i" = "available columns: {.field {names(df)}}"
      ),
      call = call
    )
  }
}
