# Note: should this be implemented as S3 method?
#' Retrieve parsing problems
#'
#' This function retrieves parsing problems encountered during the reading and processing of files.
#'
#' @rdname problems
#' @param obj data object that also has problems information
#' @param strip_ansi whether to remove ansi characters from the message, yes by default
#' @return tibble data frame with a list of problems encountered during processing
#' @export
orbi_get_problems <- function(obj, strip_ansi = TRUE) {
  # safety
  check_arg(
    obj,
    !missing(obj) &&
      (is.list(obj) || is.data.frame(obj)),
    "must be a list or data frame"
  )

  # what type of data is it?
  if (is(obj, "orbi_aggregated_data")) {
    # aggregated data
    probs <-
      obj |>
      orbi_get_data(
        file_info = c("file" = "filepath"),
        problems = everything()
      ) |>
      dplyr::mutate(file = basename(.data$file)) |>
      suppressMessages()
  } else if (is(obj, "orbi_raw_files")) {
    # raw file data
    probs <-
      obj |>
      dplyr::mutate(
        uidx = dplyr::row_number(),
        file = basename(.data$filepath)
      ) |>
      dplyr::select("uidx", "file", "problems") |>
      tidyr::unnest("problems")
  } else {
    # everything else
    probs <- tibble(
      type = character(),
      call = character(),
      message = character(),
      condition = list()
    )

    # check for elements (should this search recursively?)
    if (any(c("problems", "conditions") %in% names(obj))) {
      new_probs <- if ("problems" %in% names(obj)) {
        obj$problems
      } else {
        obj$conditions
      }
      # convert if it's a list of dfs
      if (!is.data.frame(probs) && is.list(probs)) {
        new_probs <- dplyr::bind_rows(new_probs)
      }
      probs <- dplyr::bind_rows(probs, new_probs)
    }

    # check for attributes
    if (!is.null(attr(obj, "problems"))) {
      probs <- dplyr::bind_rows(probs, attr(obj, "problems"))
    }
  }

  # strip ansi?
  if (strip_ansi) {
    probs <- probs |> dplyr::mutate(message = ansi_strip(.data$message))
  }

  # return
  return(probs)
}

#' Show parsing problems
#'
#' This function prints out parsing problems encountered during the reading and processing of files.
#'
#' @rdname problems
#' @param obj data object that holds problems information
#' @examples
#'
#' orbi_find_raw(system.file("extdata", package = "isoorbi"))[1] |>
#'   orbi_read_raw() |>
#'   orbi_show_problems()
#'
#' @export
orbi_show_problems <- function(obj) {
  orbi_get_problems(obj, strip_ansi = FALSE) |> show_cnds()
}
