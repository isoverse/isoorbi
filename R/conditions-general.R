# assertions -----

# per tidyverse: just use normal if/else conditions and then use cli_abort with glue syntax

# condition handling -----------

# notes: evaluate package can do a lot of this but is overkill (desigend for running source files)
# looks something like this
# info <- evaluate::evaluate(my_expr)
# tibble(condition = as.list(info[purrr::map_lgl(info, ~is(.x, "warning") || is(.x, "error"))])) |>
# mutate(
#    type = purrr::map_lgl(.data$condition, is, "error") |> ifelse("error", "warning"),
#    call = purrr::map_chr(.data$condition, ~as.character(conditionCall(.x))[1]),
#    message = purrr::map_chr(.data$condition, conditionMessage))

# Try/catch for executing functions that should not disrupt overall flow
# this is somewhat similar to purrr::safely but has more details summary
# @param error_value which value to return if an error is caught
# @param catch_errors whether to catch errors (vs. throwing them)
# @param catch_warnigns whether to catch warnings (vs. throwing them)
# @param truncate_call_stack whether to omit the try_catch_cnds calls from the resulting call stack in errors
# @param eval_tidy whether expression needs to be evaluated with tidy eval (in the passed in env and with data, if provided)
# @return list with result and conditions, use show_cnds(out$conditions) to show conditions if any were caught
try_catch_cnds <- function(
  expr,
  error_value = NULL,
  catch_errors = TRUE,
  catch_warnings = TRUE,
  truncate_call_stack = TRUE,
  parent_call = rlang::caller_call()
) {
  conds <- tibble::tibble(type = character(0), condition = list())

  handle_warning <- function(cnd) {
    # add warning
    conds <<- conds |>
      dplyr::bind_rows(tibble::tibble(type = "warning", condition = list(cnd)))
    rlang::cnd_muffle(cnd)
  }

  handle_error <- function(cnd) {
    # truncate call stack to omit the helper function?
    if (truncate_call_stack && is(cnd$trace, "rlang_trace")) {
      is_helper_start <- cnd$trace$call |>
        sapply(function(x) as.character(x)[1] == "try_catch_cnds")
      is_helper_end <- cnd$trace$call |>
        sapply(function(x) as.character(x)[1] == "withCallingHandlers")
      # found start and end? --> remove helpers from call stack
      if (
        any(is_helper_start) && any(is_helper_end[cumsum(is_helper_start) > 0])
      ) {
        # for each helper (i.e. also works in nested try_catch_cnds calls)
        total_shift <- 0
        for (first_call in which(is_helper_start)) {
          last_call <- first_call +
            which(is_helper_end[seq_along(is_helper_end) > first_call])[1]
          if (last_call > first_call) {
            shift <- last_call - first_call + 1L
            cnd$trace <-
              cnd$trace |>
              # correct parent references
              dplyr::mutate(
                parent = ifelse(
                  .data$parent > last_call - total_shift,
                  .data$parent - shift,
                  .data$parent
                )
              ) |>
              # omit the helper callstack
              dplyr::filter(
                dplyr::row_number() < first_call - total_shift |
                  dplyr::row_number() > last_call - total_shift
              )
            total_shift <- total_shift + shift
          }
        }
      }
    }

    # re-throw error or catch it?
    if (!catch_errors) {
      rlang::abort(
        message = NULL,
        parent = cnd,
        class = class(cnd),
        call = parent_call
      )
    } else {
      conds <<- conds |>
        dplyr::bind_rows(tibble::tibble(type = "error", condition = list(cnd)))
    }
    return(error_value)
  }

  # don't catch warnings if not wanted to make sure they get handled as originally intended
  # still keeps the withCallingHandlers call to deal with callstack truncation properly
  if (catch_warnings) {
    result <- tryCatch(
      error = handle_error,
      withCallingHandlers(
        warning = handle_warning,
        expr
      )
    )
  } else {
    result <- tryCatch(error = handle_error, withCallingHandlers(expr))
  }

  # format conditions nicely
  conds <- conds |>
    dplyr::mutate(
      call = .data$condition |>
        purrr::map_chr(~ as.character(conditionCall(.x))[1]),
      message = .data$condition |>
        # strip ansi since this is just the problems summary
        purrr::map_chr(
          ~ gsub("\\n", " ", cli::ansi_strip(conditionMessage(.x)))
        ),
      .before = "condition"
    )

  # return
  return(list(result = result, conditions = conds))
}

# summarize cnds, i.e. how many issues/errors in cli format
summarize_cnds <- function(conditions, format = "{issues}") {
  if (nrow(conditions) == 0) {
    return("")
  }
  issues <- c()
  if (n <- sum(conditions$type == 'warning')) {
    issues <- format_inline("{col_yellow(format_inline('{n} warning{?s}'))}")
  }
  if (n <- sum(conditions$type == 'error')) {
    issues <- c(
      issues,
      format_inline("{col_red(format_inline('{n} error{?s}'))}")
    )
  }
  summary <- format_inline(format)
  return(summary)
}

# helper to cli format conditions into a bullet list
# @param conditions conditions data frame generated by try_catch_cnds
# @param include_call whether to include the issuing call (if available)
format_cnds <- function(
  conditions,
  include_symbol = TRUE,
  include_call = TRUE,
  prefix = "",
  call_prefix = "in ",
  indent = 0
) {
  if (nrow(conditions) == 0L) {
    return(c())
  }
  indent <- rep("\u00a0", indent * 2) |> paste(collapse = "")
  out <- conditions |>
    mutate(
      symbol = ifelse(
        .data$type == "error",
        "{col_red(symbol$cross)} ",
        "{col_yellow('!')} "
      ),
      call_label = ifelse(
        !is.na(.data$call),
        sprintf("%s{.fn %s}: ", call_prefix, .data$call),
        ""
      ),
      message_w_type = paste0(
        indent,
        prefix,
        if (include_symbol) .data$symbol,
        if (include_call) .data$call_label,
        .data$message
      )
    ) |>
    pull(.data$message_w_type)
  return(out)
}

# Show caught conditions
# prints out the caught conditions in a cli_bullets list using summarize_cnds and format_cnds
# @param include_summary whether to include a summary (only relevant if there are any conditions)
# @param summary_message message to include in the summary, only relevant if include_summary is TRUE and there are any conditions
# @param include_call whether to include the caller of show_cnds in the summary, only relevant if include_summary is TRUE and there are any conditions
# @param indent_cnds whether to indent the list of cnds by default yes if summary is included
# @param include_cnd_calls whether to include information on the cnd calls
# @param call only used if include_summary = list(call = TRUE)
# only prints if there
show_cnds <- function(
  conditions,
  include_summary = TRUE,
  summary_message = NULL,
  include_call = include_summary,
  indent_cnds = include_summary,
  include_cnd_calls = TRUE,
  call = rlang::caller_call()
) {
  # safety
  force(call)
  if (missing(conditions) || !is.data.frame(conditions)) {
    cli_abort("{.var conditions} must be provided as a data frame")
  }

  # summary
  summary_line <- NULL
  has_summary <- include_summary && nrow(conditions) > 0
  if (has_summary) {
    summary_line <- summarize_cnds(conditions)
    # summary_message
    if (!is.null(summary_message)) {
      summary_line <- paste(summary_line, summary_message)
    }
    # call
    if (include_call) {
      summary_line <- paste("in {.fn {as.character(call)[1]}}:", summary_line)
    }
  }

  # single issue and summary --> make one line for both
  if (has_summary && nrow(conditions) == 1L) {
    # single condition
    summary_line <- paste(
      summary_line,
      "{symbol$arrow_right}",
      format_cnds(
        conditions,
        include_call = include_cnd_calls,
        include_symbol = FALSE,
        call_prefix = ""
      )
    )
    formatted_cnds <- c()
  } else {
    # multiple conditions
    formatted_cnds <- conditions |>
      format_cnds(
        include_call = include_cnd_calls,
        indent = indent_cnds,
        prefix = if (indent_cnds) "{symbol$arrow_right} " else ""
      )
  }

  # summary icon
  if (has_summary) {
    summary_icon <-
      if (nrow(conditions) == 0L) {
        "v"
      } else if (any(conditions$type == "error")) {
        "x"
      } else {
        "!"
      }
    summary_line <- setNames(summary_line, summary_icon)
  }

  # output
  cli_bullets(c(summary_line, formatted_cnds))
}
