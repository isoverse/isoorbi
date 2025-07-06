#' Retrieve parsing problems
#'
#' This function retrieves parsing problems encountered during the reading and processing of files.
#'
#' @param obj data object that holds problems information
#' @examples
#'
#' orbi_find_raw(system.file("extdata", package = "isoorbi"))[1] |>
#'   orbi_read_raw() |>
#'   orbi_get_problems()
#'
#' @return tibble data frame with a list of problems encountered during
#' @export
orbi_get_problems <- function(obj) {
  stopifnot(
    "`obj` must be a list or data frame" = !missing(obj) &&
      (is.list(obj) || is.data.frame(obj))
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
  if ("problems" %in% names(obj)) {
    new_probs <- obj$problems
    # convert if it's a list of dfs
    if (!is.data.frame(probs) && is.list(probs)) {
      new_probs <- dplyr::bind_rows(new_probs)
    }
    probs <- dplyr::bind_rows(probs, new_probs)
  }

  # check for attributes
  if (!is.null(attr(obj, "problems"))) {
    probs <- bind_rows(probs, attr(obj, "problems"))
  }

  # return
  return(probs)
}
