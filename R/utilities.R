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
  stopifnot(is.data.frame(dataset), is_character(cols))
  by_cols <- tidyselect::eval_select(any_of(cols), dataset) |> names()
  if (length(by_cols) > 0) {
    dataset <- dataset |> group_by(!!!purrr::map(by_cols, sym), .add = add)
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
    dplyr::select(dplyr::all_of(c(dplyr::group_vars(dataset), column))) |>
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


# Data utility functions ======

#' Get available example file(s)
#'
#' This function will provide the path(s) to example file(s).
#' If a requested file is not yet available locally but is available on https://github.com/isoverse/isodata, it will download it from there into local storage.
#' By default, it will download only cache files (.raw.cache.zip) instead of the original .raw files because the cache files are significantely smaller.
#' Todownload the original raw files instead, use `download_raw_files = TRUE`.
#'
#' @param filenames names of the example files
#' @param download_raw_files should the original raw files be downloaded? By default only cache files (raw.cache.zip) are downloaded as they are usually much smaller.
#' However, they will not work for retrieving additional spectra. To download the original spectra, switch to `download_raw_files = TRUE`
#' @param download_always whether to download files anew even if they already exist locally
#' @return file path(s) that can be passed directly to [orbi_read_raw()]
#' @export
orbi_get_example_files <- function(
  filenames,
  download_raw_files = FALSE,
  download_always = FALSE
) {
  # safety checks
  check_arg(
    filenames,
    !missing(filenames) && is_character(filenames) && length(filenames) > 0,
    "must be at leat one filename"
  )
  check_arg(
    download_raw_files,
    is_scalar_logical(download_raw_files),
    "must be TRUE or FALSE (did you forget `c()` around the filenames?)"
  )
  check_arg(
    download_always,
    is_scalar_logical(download_always),
    "must be TRUE or FALSE"
  )

  # locations
  srcdir <- "https://github.com/isoverse/isodata/raw/refs/heads/main/isoorbi/"
  sysdir <- system.file("extdata", package = "isoorbi")
  syspaths <- file.path(sysdir, filenames)
  destdir <- tools::R_user_dir("isoorbi", which = "data")
  destpaths <- file.path(destdir, filenames)
  info <- get_file_paths_info(destpaths)
  for (i in seq_along(info$idx)) {
    if (file.exists(syspaths[i])) {
      # file exists in package system directory
      destpaths[i] <- syspaths[i]
    } else if (
      download_always ||
        !info$is_cached[i] ||
        (download_raw_files && !info$file_exists[i])
    ) {
      # need to download file
      if (!dir.exists(destdir)) {
        dir.create(destdir, recursive = TRUE)
      }
      dest_path <- if (download_raw_files) {
        info$file_path[i]
      } else {
        info$cache_path[i]
      }
      if (file.exists(dest_path)) {
        unlink(dest_path)
      }
      start_info("downloading {cli::col_blue(basename(dest_path))}")
      download_path <- paste0(srcdir, basename(dest_path))
      out <- download_path |>
        utils::download.file(destfile = dest_path, quiet = TRUE) |>
        try_catch_cnds()
      finish_info()
      show_cnds(
        out$conditions,
        message = format_inline(
          "there was a problem downloading {cli::col_blue(basename(dest_path))}, "
        )
      )
    }
  }
  return(destpaths)
}
