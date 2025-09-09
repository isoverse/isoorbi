#' Package options
#'
#' These options are best set via [orbi_options()] and queried via [orbi_get_option()]. However, the base functions [options()] and [getOption()] work as well but require an `isoorbi.` prefix (the package name and a dot) for the option name. Setting an option to a value of `NULL` means that the default is used. [orbi_get_options()] is available as an additional convenience function to retrieve a subset of options with a regular expression pattern.
#'
#' @examples
#' # All default options
#' orbi_get_options()
#'
#' # Options that contain 'data' in the name
#' orbi_get_options("data")
#'
#' # Specific option
#' orbi_get_option("data_type_unused")
#'
#' # Change an option
#' orbi_options(data_type_unused = "flagged")
#' orbi_get_option("data_type_unused")
#'
#' # Change back to default
#' orbi_options(data_type_unused = NULL)
#' orbi_get_option("data_type_unused")
#'
#' @param ... set package options, syntax identical to [options()]
#' @describeIn orbi_options set/get option values
#' @export
orbi_options <- function(...) {
  pkg_options(pkg = "isoorbi", pkg_options = get_pkg_options(), ...)
}

#' @param pattern to retrieve multiple options (as a list) with a shared pattern
#' @describeIn orbi_options get a subset of option values that fit a pattern
#' @export
orbi_get_options <- function(pattern = NULL) {
  pkg_options <- orbi_options()
  if (!is.null(pattern)) {
    pkg_options <- pkg_options[grepl(pattern, names(pkg_options))]
  }
  return(pkg_options)
}

#' @describeIn orbi_options retrieve the current value of one option (option must be defined for the package)
#' @param x name of the specific option to retrieve
#' @export
orbi_get_option <- function(x) {
  get_pkg_option(option = x, pkg = "isoorbi", pkg_options = get_pkg_options())
}

#' @rdname orbi_options
#' @format NULL
#' @usage NULL
#' @section Options for the isoorbi package:
get_pkg_options <- function() {
  list(
    #' - `di_ref_name`: the text label for dual inlet reference blocks
    di_ref_name = define_pkg_option(
      default = "ref",
      check_fn = is_scalar_character
    ),
    #' - `di_sample_name`: the text label for dual inlet sample blocks
    di_sample_name = define_pkg_option(
      default = "sam",
      check_fn = is_scalar_character
    ),
    #' - `data_type_data`: the text used to flag raw data as actually being data
    data_type_data = define_pkg_option(
      default = "data",
      check_fn = is_scalar_character
    ),
    #' - `data_type_startup`: the text used to flag raw data as being part of the startup
    data_type_startup = define_pkg_option(
      default = "startup",
      check_fn = is_scalar_character
    ),
    #' - `data_type_changeover`: the text used to flag raw data as being part of a changeover
    data_type_changeover = define_pkg_option(
      default = "changeover",
      check_fn = is_scalar_character
    ),
    #' - `data_type_unused`: the text used to flag raw data as being unused
    data_type_unused = define_pkg_option(
      default = "unused",
      check_fn = is_scalar_character
    ),
    #' - `raw_aggregator`: configuration for pulling data out of raw files
    raw_aggregator = define_pkg_option(
      default = orbi_start_aggregator(
        "file_info",
        uid_source = "RAW file",
        cast = "as.factor"
      ) |>
        orbi_add_aggregator(
          "file_info",
          "\\1",
          source = "(.*)",
          regexp = TRUE
        ) |>
        orbi_add_aggregator("scans", "scan", cast = "as.integer") |>
        orbi_add_aggregator(
          "scans",
          "time.min",
          source = "StartTime",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator(
          "scans",
          "tic",
          source = "TIC",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator(
          "scans",
          "it.ms",
          source = "Ion Injection Time (ms):",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator(
          "scans",
          "resolution",
          source = c("FT Resolution:", "Orbitrap Resolution:"),
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator(
          "scans",
          "basePeakIntensity",
          source = "basePeak",
          cast = "as.numeric",
          func = "sapply",
          args = list(`[`, 2)
        ) |> # list column that needs 2nd value
        orbi_add_aggregator(
          "scans",
          "rawOvFtT",
          source = "RawOvFtT:",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator(
          "scans",
          "intensCompFactor",
          source = "OT Intens Comp Factor:",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator("scans", "agc", source = "AGC:") |>
        orbi_add_aggregator(
          "scans",
          "agcTarget",
          source = "AGC Target:",
          cast = "as.integer"
        ) |>
        orbi_add_aggregator(
          "scans",
          "microscans",
          source = "Micro Scan Count:",
          cast = "as.integer"
        ) |>
        orbi_add_aggregator(
          "scans",
          "numberLockmassesFound",
          source = "Number of LM Found:",
          cast = "as.integer"
        ) |>
        orbi_add_aggregator(
          "scans",
          "analyzerTemperature",
          source = "Analyzer Temperature:",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator("peaks", "scan", cast = "as.integer") |>
        orbi_add_aggregator(
          "peaks",
          "mzMeasured",
          source = "centroid.PreferredMasses",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator(
          "peaks",
          "intensity",
          source = "centroid.intensity",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator(
          "peaks",
          "peakNoise",
          source = "centroid.PreferredNoises",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator("spectra", "scan", cast = "as.integer") |>
        orbi_add_aggregator(
          "spectra",
          "mz",
          source = "mZ",
          cast = "as.numeric"
        ) |>
        orbi_add_aggregator("spectra", "intensity", cast = "as.numeric"),
      check_fn = function(x) {
        if (missing(x) || !is.data.frame(x)) {
          cli_abort("{.var raw_aggregator} is not a data frame")
        }
        aggregator_req_cols <- c(
          "dataset",
          "column",
          "source",
          "default",
          "cast",
          "regexp",
          "func",
          "args"
        )
        if (length(missing <- setdiff(aggregator_req_cols, names(x)))) {
          cli_abort(
            "{.var raw_aggregator} is missing required column{?s} {.var {missing}}"
          )
        }
        return(TRUE)
      }
    ),
    #' - `debug`: turn on debug mode
    debug = define_pkg_option(default = FALSE, check_fn = is_scalar_logical),
    #' - `auto_use_ansi`: whether to automatically enable correct rendering of stylized (ansi) output in HTML reports from notebooks that call `library(isoorbi)`. Can be turned off by calling `isoorbi::orbi_options(auto_use_ansi = FALSE)` **before** call `library(isoorbi)`.
    auto_use_ansi = define_pkg_option(
      default = TRUE,
      check_fn = is_scalar_logical
    )
  )
}

# deprecated ------------------------

#' Set package settings
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `orbi_set_settings()` was renamed `orbi_options()` as part of `isoorbi` switching from 'settings' to 'options' to be consistent with base R naming conventions
#' @param ... named arguments to set specific options, passed on to [orbi_options()]
#' @export
orbi_set_settings <- function(...) {
  lifecycle::deprecate_warn(
    "1.4.0",
    "orbi_set_settings()",
    "orbi_options()",
    details = "`isoorbi` switched from 'settings' to 'options' to be consistent with base R naming conventions"
  )
  return(invisible(orbi_options(...)))
}

#' Get all isoorbi package settings
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `orbi_get_settings()` was renamed `orbi_get_options()` as part of `isoorbi` switching from 'settings' to 'options' to be consistent with base R naming conventions
#' @param pattern passed on to [orbi_get_options()]
#' @export
orbi_get_settings <- function(pattern = NULL) {
  lifecycle::deprecate_warn(
    "1.4.0",
    "orbi_get_settings()",
    "orbi_get_options()",
    details = "`isoorbi` switched from 'settings' to 'options' to be consistent with base R naming conventions"
  )
  return(orbi_get_options(pattern))
}
