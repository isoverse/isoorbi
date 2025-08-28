#' Retrieve parsing problems
#'
#' This function retrieves parsing problems encountered during the reading and processing of files.
#'
#' @rdname problems
#' @param obj data object that holds problems information
#' @param strip_ansi whether to remove ansi characters from the message, yes by default
#' @examples
#'
#' orbi_find_raw(system.file("extdata", package = "isoorbi"))[1] |>
#'   orbi_read_raw() |>
#'   orbi_get_problems()
#'
#' @return tibble data frame with a list of problems encountered during
#' @export
orbi_get_problems <- function(obj, strip_ansi = TRUE) {
  # safety
  check_arg(
    obj,
    !missing(obj) &&
      (is.list(obj) || is.data.frame(obj)),
    "must be a list or data frame"
  )

  # template
  probs <- tibble(
    uid = factor(),
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
