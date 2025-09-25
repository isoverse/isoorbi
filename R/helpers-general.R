# general utility functions (any package) =======

# Create factor in order
#
# Helper function to create factors with levels in the order of data appearance. This is a simpler implementation of [forcats::fct_inorder()]
# @param x vector or factor
# @return factor with levels in order of appearance
factor_in_order <- function(x) {
  if (!is.factor(x)) {
    x <- as.factor(x)
  }
  idx <- as.integer(x)[!duplicated(x)]
  idx <- idx[!is.na(idx)]
  return(factor(x, levels = levels(x)[idx]))
}

# check function argument for condition (instead of stopifnot) for more informative error messages
# note: throws error if `condition` evaluates to FALSE
check_arg <- function(
  x,
  condition,
  msg,
  include_type = TRUE,
  include_value = FALSE,
  .arg = caller_arg(x),
  .env = caller_env()
) {
  if (!condition) {
    if (is_missing(maybe_missing(x))) {
      type = if (include_type) ", not missing" else ""
      cli_abort("argument {.field {(.arg)}} {msg}{type}", call = .env)
    } else {
      type <- if (include_type) {
        format_inline(", not {.obj_type_friendly {x}}")
      } else {
        ""
      }
      value <- if (include_value) format_inline(" ({.val {x}})") else ""
      cli_abort(
        "argument {.field {(.arg)}}{value} {msg}{type}",
        call = .env,
        trace = trace_back(bottom = .env)
      )
    }
  }
}

# check tibble for being a tibble and required columns if there are any
check_tibble <- function(
  df,
  req_cols = c(),
  regexps = FALSE,
  .arg = caller_arg(df),
  .env = caller_env()
) {
  check_arg(
    df,
    !missing(df) && is.data.frame(df),
    "must be a data frame or tibble",
    .arg = .arg,
    .env = .env
  )

  # missing
  if (regexps) {
    fits <- purrr::map_lgl(
      req_cols,
      ~ {
        grepl(
          paste0("^(", .x, ")$"),
          names(df)
        ) |>
          any()
      }
    )
    missing <- req_cols[!fits]
  } else {
    missing <- setdiff(req_cols, names(df))
  }

  if (length(missing) > 0) {
    cli_abort(
      c(
        "{qty(missing)} column{?s} {.field {missing}} {?is/are} missing from {.field {(.arg)}}",
        "i" = "available columns: {.field {names(df)}}"
      ),
      call = .env
    )
  }
}

# print out info start message
# @param ... message pieces
# @param func whether to include the function name
# @param keep whether to keep the start info displayed (or remove via progress bar)
# @param pb_... parameters passed to cli_progress_bar if there is one
start_info <- function(
  ...,
  func = TRUE,
  keep = FALSE,
  pb_type = "tasks",
  pb_total = NA,
  pb_extra = NULL,
  pb_status = NULL,
  show_progress = rlang::is_interactive(),
  .env = caller_env(),
  .call = caller_call()
) {
  # safety
  stopifnot(is_scalar_logical(func), is_scalar_logical(keep))

  # call
  call <- as.character(.call[1])
  if (is.null(.call[1])) {
    func <- FALSE # con't include if it's NULL
  }

  # message
  msg <- c(
    if (func) sprintf("{.strong %s()} ", call),
    ...,
    "..."
  )
  retval <- list(pb = NULL, start_time = Sys.time())
  if (...length() == 0) {
    # no message, just return the start time
  } else if (keep) {
    # message is permanent
    cli_text(c("{cli::col_blue(cli::symbol$info)} ", msg), .envir = .env)
  } else if (show_progress) {
    # message is a progress bar (only in interactive mode)
    retval$pb <- cli_progress_bar(
      format = c("{cli::pb_spin} ", msg),
      type = pb_type,
      total = pb_total,
      extra = pb_extra,
      status = pb_status,
      # make sure it closes if process in .env fails
      .auto_close = TRUE,
      .envir = .env
    )
    cli_progress_update(id = retval$pb, inc = 0, force = TRUE, .envir = .env)
  }
  return(invisible(retval))
}

# print out info end message
# @param ... message pieces
# @param start the progress bar and start_time info (if there is any)
# @param time whether to include time (used to disable time displays during snapshot testing)
# @param func whether to include the function name
# @param conditions any conditions that might have been encountered
# @param show_conditions whether to show conditions if any have been encountered
# @param abort_if_warnings abort if there are any warnings in conditions
# @param abort_if_errors abort if there are any errors in conditions
finish_info <- function(
  ...,
  start = list(pb = NULL, start_time = NULL),
  time = getOption("show_exec_times", default = TRUE),
  func = TRUE,
  success_format = "{cli::col_green(symbol$tick)} {msg}",
  conditions = tibble(),
  show_conditions = TRUE,
  abort_if_warnings = abort_if_errors,
  abort_if_errors = FALSE,
  .env = caller_env(),
  .call = caller_call()
) {
  # safety
  stopifnot(
    is_scalar_logical(func),
    is.data.frame(conditions) &&
      (nrow(conditions) == 0 || "type" %in% names(conditions))
  )

  # close progress bar if there is one
  # .auto_close does this also if there's a failure / .env finishes
  # but it's cleaner to do it here before the finish message
  if (!is.null(start$pb)) {
    cli_progress_done(id = start$pb, .envir = .env)
  }

  # call
  call <- as.character(.call[1])
  if (is.null(.call[1])) {
    func <- FALSE # con't include if it's NULL
  }

  # assemble message
  msg <-
    paste(
      if (time && !is.null(start$start_time)) {
        format_inline(
          "{.timestamp {prettyunits::pretty_sec(as.numeric(Sys.time() - start$start_time, 'secs'))}}"
        )
      },
      if (func) format_inline("{.strong {call}()}"),
      format_inline(..., .envir = .env)
    )

  # output finish info
  if (nrow(conditions) > 0) {
    # check if there's reason to abort
    if (
      (abort_if_warnings && any(conditions$type == "warning")) ||
        (abort_if_errors && any(conditions$type == "error"))
    ) {
      # abort
      abort_cnds(
        conditions,
        message = msg,
        include_call = FALSE,
        summary_format = "{message} but encountered {issues}",
        include_cnds = TRUE,
        .call = .call
      )
    }

    # summarize/show issues
    show_cnds(
      conditions,
      message = msg,
      include_call = FALSE,
      summary_format = "{message} but encountered {issues}",
      include_cnds = show_conditions,
      .call = .call
    )
  } else if (...length() > 0) {
    # success message
    cli_text(success_format)
  }
}
