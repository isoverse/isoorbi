# Internal utility functions =============

# internal function to check dataset argument
check_dataset_arg <- function(
  dataset,
  .arg = caller_arg(dataset),
  .env = caller_env()
) {
  # safety checks
  check_arg(
    dataset,
    !missing(dataset) &&
      (is(dataset, "orbi_aggregated_data") ||
        is.data.frame(dataset)),
    "must be a set of aggregated raw files or a data frame with peaks",
    .arg = .arg,
    .env = .env
  )
}

# internal function to ensure dataset columns are factors (if they exist)
# will notify the user about the transformation
factorize_dataset <- function(dataset, cols = c()) {
  factor_cols <- names(dataset)[purrr::map_lgl(dataset, is.factor)]
  factorize_cols <- setdiff(cols, factor_cols) |> intersect(names(dataset))
  if (length(factorize_cols) > 0) {
    for (col in factorize_cols) {
      dataset[[col]] <- factor_in_order(dataset[[col]])
    }
    cli_inform(
      c(
        "i" = "column{?s} {.field {factorize_cols}} {?was/were} turned into {?a/} factor{?s}"
      )
    )
  }
  return(dataset)
}

# group if exists
group_if_exists <- function(dataset, cols, add = TRUE) {
  for (col in cols) {
    if (col %in% names(dataset)) {
      dataset <- dataset |> group_by(!!sym(col), .add = add)
    }
  }
  return(dataset)
}

# group one dataset the same as another (use to restore original groupings)
group_by_same_groups <- function(target_dataset, source_dataset) {
  target_dataset |> dplyr::group_by(!!!dplyr::groups(source_dataset))
}

# count distinct column respecting existing grouping
count_grouped_distinct <- function(dataset, column) {
  dataset |>
    dplyr::select(!!!c(dplyr::groups(dataset), column)) |>
    dplyr::distinct() |>
    dplyr::count(!!sym(column)) |>
    dplyr::pull(.data$n) |>
    sum()
}

# format number pretty number
format_number <- function(x) {
  x |>
    prettyunits::pretty_num() |>
    # take care of leading/trailing whitespaces (style = "no_pad") doesn't quite do this
    #gsub(pattern = "(^ +| +$)", replacement = "")
    # remove all white spacs (also between the units)
    gsub(pattern = " ", replacement = "", fixed = TRUE)
}
