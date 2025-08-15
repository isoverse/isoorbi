# general utility functions (any package) =======

# check function argument for condition (instead of stopifnot) for more informative error messages
check_arg <- function(
  x,
  condition,
  msg,
  arg = caller_arg(x),
  call = caller_env(),
  include_type = TRUE,
  include_value = FALSE
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
      value <- if (include_value) format_inline(" ({.val {x}})") else ""
      cli_abort("argument {.field {arg}}{value} {msg}{type}", call = call)
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

# print out info start message
# @param ... message pieces
# @param func whether to include the function name
# @param keep whether to keep the start info
start_info <- function(
  ...,
  func = TRUE,
  keep = FALSE,
  env = caller_env(),
  call = caller_call()
) {
  # prefix
  prefix <- "{col_green(symbol$info)} "
  if (func) {
    prefix <- sprintf("%s{.strong %s()} ", prefix, as_label(call[[1]]))
  }
  msg <- c(prefix, ..., "...")
  retval <- list(pb = NULL, start_time = Sys.time())
  if (...length() == 0) {
    # no message, just return the start time
  } else if (keep) {
    # message is permanent
    cli_text(msg, .envir = parent.frame())
  } else {
    # message is a progress message
    retval$pb <- cli_progress_message(msg, .envir = parent.frame())
  }
  return(invisible(retval))
}

# print out info end message
finish_info <- function(
  ...,
  start = list(pb = NULL, start_time = NULL),
  func = TRUE,
  env = caller_env(),
  call = caller_call()
) {
  # close progress bar if there is one
  if (!is.null(start$pb)) {
    cli_progress_done(id = start$pb, .envir = parent.frame())
  }

  # are there any final messages to print?
  if (...length() > 0) {
    # prefix
    prefix <- format_inline(
      "{col_green(symbol$tick)} {.timestamp {prettyunits::pretty_sec(as.numeric(Sys.time() - start$start_time, 'secs'))}} "
    )
    if (func) {
      prefix <- sprintf("%s{.strong %s()} ", prefix, as_label(call[[1]]))
    }

    # using `cli_text()` instead of `cli_alert_success()` because it wraps to output width
    cli_text(prefix, ..., .envir = parent.frame())
  }
}
