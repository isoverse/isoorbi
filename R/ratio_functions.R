# Functions to calculate direct ratios --------------

#' @title Calculate direct isotopocule ratios
#' @description This function calculates isotopocule/base peak ratios for all isotopocules. It does not summarize or average the ratios in any way. For a summarizing version of this function, see `orbi_summarize_results()`.
#' @param dataset A data frame output after running `orbi_define_basepeak()`
#' @return Returns a mutated dataset with `ratio` column added.
#' @export
orbi_calculate_ratios <- function(dataset) {

  # safety checks
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `isotopocule` and `ions.incremental`" =
      all(c("isotopocule", "ions.incremental") %in% names(dataset)),
    "`dataset` requires defined basepeak (columns `basepeak` and `basepeak_ions`), make sure to run `orbi_define_basepeak()` first" =
      all(c("basepeak", "basepeak_ions") %in% names(dataset))
  )

  # deprecated
  lifecycle::deprecate_warn(
    "1.3.0", "orbi_calculate_ratios()", 
    details = "this function is obsolete as `orbi_define_basepeak()` already calculates the ratios automatically",
    always = TRUE
  )
  
  # info message
  start_time <-
    sprintf(
      "orbi_calculate_ratios() is calculating all isotopocule/base peak ratios... "
    ) |>
    message_start()

  # mutate
  dataset <- dataset |> factorize_dataset("isotopocule")

  dataset <-
    try_catch_all(
      dataset |>
        dplyr::mutate(
          ratio = .data$ions.incremental / .data$basepeak_ions,
          .after = "ions.incremental"
        ),
      "something went wrong calculating ratios: "
    )

  # info message
  sprintf(
    "calculated %d ratios for %d isotopocules/base peak (%s)",
    nrow(dataset),
    length(levels(dataset$isotopocule)),
    paste(levels(dataset$isotopocule), collapse = ", ")
  ) |> message_finish(start_time = start_time)

  # return
  return(dataset)
}

# Functions to calculate summarized ratios and stats --------------------------------------------

# @title Internal function to calculate standard error
# @description The function `calculate_ratios_sem()` computes a regular standard error.
# @keywords internal
# @param ratios A numeric vector used to calculate a standard error
# @return The calculated standard error
calculate_ratios_sem <- function(ratios) {

  # safety checks
  stopifnot(
    "no input vector for `ratios` supplied" = !missing(ratios),
    "`ratios` need to be provided as a numeric vector" = is.vector(ratios) && is.numeric(ratios),
    "length of `ratios` needs to be > 1, cannot calculate SEM with a single value" = length(ratios) > 1L
  )

  # calculation
  try_catch_all(
    stats::sd(ratios) / sqrt(length(ratios)),
    "something went wrong calculating the standard error: "
  )
}

# @title Internal function to calculate geometric mean
# @description  The function `calculate_ratios_gmean()` is used to calculate geometric means.
# @keywords internal
# @param ratios A numeric vector of ratios used to calculate the geometric mean
# @return The calculated geometric mean
calculate_ratios_gmean <- function(ratios) {

  # safety checks
  stopifnot(
    "no input vector for `ratios` supplied" = !missing(ratios),
    "`ratios` need to be provided as a numeric vector" = is.vector(ratios) && is.numeric(ratios)
  )

  # calculation
  try_catch_all(
    exp(mean(log(ratios))),
    "something went wrong calculating the geometic mean: "
  )
}


# @title Internal function to calculate standard deviation (geometric)
# @description  The function `calculate_ratios_gsd()` is used to calculate geometric standard deviations.
# @keywords internal
# @param ratios A numeric values used to calculate the geometric standard deviation
# @return The calculated geometric standard deviation
calculate_ratios_gsd <- function(ratios) {

  # safety checks
  stopifnot(
    "no input vector for `ratios` supplied" = !missing(ratios),
    "`ratios` need to be provided as a numeric vector" = is.vector(ratios) && is.numeric(ratios),
    "length of `ratios` needs to be > 1, cannot calculate GSD with a single value" = length(ratios) > 1L
  )

  try_catch_all(
    exp(mean(log(ratios)) + stats::sd(log(ratios))) - exp(mean(log(ratios))),
    "something went wrong calculating geometric standard deviaton: "
  )

}

# @title Internal function to calculate standard error (geometric)
# @description  The function `calculate_ratios_gse()` is used to calculate geometric standard errors.
# @keywords internal
# @param ratios A vector of values used to calculate geometric standard errors
# @return The calculated geometric standard error
calculate_ratios_gse <- function(ratios) {

  # safety checks
  stopifnot(
    "no input vector for `ratios` supplied" = !missing(ratios),
    "`ratios` need to be provided as a numeric vector" = is.vector(ratios) && is.numeric(ratios),
    "length of `ratios` needs to be > 1, cannot calculate GSE with a single value" = length(ratios) > 1L
  )

  try_catch_all(
    (exp(mean(log(ratios)) + stats::sd(log(ratios))) - exp(mean(log(ratios)))) / sqrt(length(ratios)),
    "something went wrong calculating the geometric standard error: "
  )

}

# @title Internal function for ratio_method `slope`
# @description  The function `calculate_ratios_slope()` is used to estimate the slope of x, y values used in a ratio.
# @keywords internal
# @param x Vector of values used as ratio numerator
# @param y Vector of values used as ratio denominator
# @details The slope is calculated from a linear regression model that is weighted by the numerator x, using `stats::lm(x ~ y + 0, weights = x)`
# @return The calculated slope, an estimate of the ratio x/y
calculate_ratios_slope <- function(x, y) {

  # safety checks
  stopifnot(
    "no input vector for `x` supplied" = !missing(x),
    "no input vector for `y` supplied" = !missing(y),
    "`x` needs to be provided as a numeric vector" = is.vector(x) && is.numeric(x),
    "`y` needs to be provided as a numeric vector" = is.vector(y) && is.numeric(y),
    "length of `x` needs to be > 1, cannot calculate slope with a single value" = length(x) > 1L,
    "length of `y` needs to be > 1, cannot calculate slope with a single value" = length(y) > 1L,
    "`x` and `y` need to be vectors of equal length" = length(x) == length(y)
  )

  model <-
    try_catch_all(
      # Note order of x and y to get correct slope!
      stats::lm(x ~ y + 0, weights = x),
      "something went wrong calculating the ratio as slope using a linear model: "
    )

  sl <- model$coefficients[[1]]

  return(sl)

}

# @title Internal function for ratio_method `weighted_sum`
# @description The function `calculate_ratios_weighted_sum()` is used to calculate ratios by weighted sums of x and y values.
# @keywords internal
# @param x A vector of values used as ratio numerator
# @param y A vector of values used as ratio denominator
# @details The weighing function ensures that each scan contributes equal weight to the ratio calculation,
# i.e. scans with more ions in the Orbitrap do not contribute disproportionally to the total sum of x and y that is used to calculate x/y.
# @return The calculated ratio x/y
calculate_ratios_weighted_sum <- function(x, y) {

  # safety checks
  stopifnot(
    "no input vector for `x` supplied" = !missing(x),
    "no input vector for `y` supplied" = !missing(y),
    "`x` needs to be provided as a numeric vector" = is.vector(x) && is.numeric(x),
    "`y` needs to be provided as a numeric vector" = is.vector(y) && is.numeric(y),
    "length of `x` needs to be > 1, cannot calculate slope with a single value" = length(x) > 1L,
    "length of `y` needs to be > 1, cannot calculate slope with a single value" = length(y) > 1L,
    "`x` and `y` need to be vectors of equal length" = length(x) == length(y)
  )

  df <- cbind(x, y)

  avg.ions <- (sum(df[, 1]) + sum(df[, 2])) / length(df[, 1])

  scan.ions <- (df[, 1] + df[, 2])

  weighted.x <- avg.ions / scan.ions  * as.numeric(df[, 1])
  weighted.y <- avg.ions / scan.ions  * as.numeric(df[, 2])

  ratio <-
    try_catch_all(
    # Note order of x and y to get correct slope!
    sum(weighted.x) / sum(weighted.y),
    "something went wrong calculating the ratio from weighted sums: "
  )

  return(ratio)
}


#' @title Calculate isotopocule ratio
#' @description This function calculates the ratio of two isotopocules (the `numerator` and `denominator`). This function averages multiple measurements of each using the `ratio_method` and returns a single value. Normally this function is not called directly by the user, but via the function [orbi_summarize_results()], which calculates isotopocule ratios and other results for an entire dataset.
#' @param numerator Column(s) used as numerator; contains ion counts
#' @param denominator Column used as denominator; contains ion counts
#' @param ratio_method Method for computing the ratio. **Please note well**: the formula used to calculate ion ratios matters! Do not simply use arithmetic mean. The best option may depend on the type of data you are processing (e.g., MS1 versus M+1 fragmentation). `ratio_method` can be one of the following:
#'
#'
#' * `mean`: arithmetic mean of ratios from individual scans.
#'
#' * `sum`: sum of all ions of the numerator across all scans divided by the sum of all ions observed for the denominator across all scans.
#'
#' * `geometric_mean`: geometric mean of ratios from individual scans.
#'
#' * `slope`: The ratio is calculated using the slope obtained from a linear regression model that is weighted by the `numerator x`, using `stats::lm(x ~ y + 0, weights = x)`.
#'
#' * `weighted_sum`: A derivative of the `sum` option. The weighing function ensures that each scan contributes equal weight to the ratio calculation,
#' i.e. scans with more ions in the Orbitrap do not contribute disproportionately to the total `sum` of `x` and `y` that is used to calculate `x/y`.
#'
#' @examples
#' df <-
#'   system.file("extdata", "testfile_flow.isox", package = "isoorbi") |>
#'   orbi_read_isox()
#'
#' ions_18O <- dplyr::filter(df, isotopocule == "18O")$ions.incremental
#' ions_M0 <- dplyr::filter(df, isotopocule == "M0")$ions.incremental
#'
#' orbi_calculate_summarized_ratio(
#'   numerator = ions_18O, denominator = ions_M0, ratio_method = "sum"
#' )
#'
#' orbi_calculate_summarized_ratio(
#'   numerator = ions_18O, denominator = ions_M0, ratio_method = "slope"
#' )
#'
#' @return Single value ratio between the isotopocules defined as `numerator` and `denominator` calculated using the `ratio_method`.
#'
#' @export
orbi_calculate_summarized_ratio <- function(
    numerator, denominator,
    ratio_method = c("direct", "mean", "sum", "median", "geometric_mean", "slope", "weighted_sum")) {

  # safety checks
  stopifnot(
    "no input for `numerator` supplied" = !missing(numerator),
    "no input for `denominator` supplied" = !missing(denominator),
    "`numerator` needs to be provided as a numeric vector" = is.vector(numerator) && is.numeric(numerator),
    "`denominator` needs to be provided as a numeric vector" = is.vector(denominator) && is.numeric(denominator),
    "no input for `ratio_method` supplied" = !missing(ratio_method)
  )

  # calculation
  o <-
    try_catch_all({
      if (ratio_method == "direct") {
        stopifnot("`numerator` and `denominator` must be vectors of equal length" =
                    length(numerator) == length(denominator))
        numerator / denominator
      } else if (ratio_method == "mean") {
        base::mean(numerator / denominator)
      } else if (ratio_method == "slope") {
        calculate_ratios_slope(numerator, denominator)
      } else if (ratio_method == "sum") {
        base::sum(numerator) / sum(denominator)
      } else if (ratio_method == "geometric_mean") {
        calculate_ratios_gmean(ratios = numerator / denominator)
      } else if (ratio_method == "weighted_sum") {
        calculate_ratios_weighted_sum(numerator, denominator)
      } else if (ratio_method == "median") {
        stats::median(numerator / denominator)
      } else{
        rlang::arg_match(ratio_method)
      }
    },
    "something went wrong calculating ratios:",
    newline = FALSE
    )
  return(o)
}

#' @title Generate the results table
#' @description Contains the logic to generate the results table. It passes the  `ratio_method` parameter to the [orbi_calculate_summarized_ratio()] function for ratio calculations.
#' @param dataset A tibble from `IsoX` output ([orbi_read_isox()]) and with a basepeak already defined (using `orbi_define_basepeak()`). Optionally, with block definitions ([orbi_define_blocks_for_dual_inlet()]) or even additional block segments ([orbi_segment_blocks()]).
#' @inheritParams orbi_calculate_summarized_ratio
#' @param .by additional grouping columns for the results summary (akin to dplyr's `.by` parameter e.g. in [dplyr::summarize()]). If not set by the user, all columns in the parameter's default values are used, if present in the dataset. Note that the order of these is also used to arrange the summary.
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
#' df <- orbi_read_isox(file = fpath) |>
#'       orbi_simplify_isox() |>
#'       orbi_define_basepeak("M0")  |>
#'       orbi_summarize_results(ratio_method = "sum")
#'
#' @return Returns a results summary table retaining the columns `filename`, `compound`, `isotopocule` and `basepeak` as well as the grouping columns from the `.by` parameter that are part of the input `dataset`. Additionally this function adds the following results columns: `start_scan.no`, `end_scan.no`, `start_time.min`, `mean_time.min`, `end_time.min`, `ratio`, `ratio_sem`, `ratio_relative_sem_permil`, `shot_noise_permil`, `No.of.Scans`, `minutes_to_1e6_ions`
#'
#' * `ratio`: The isotope ratio between the `isotopocule` and the `basepeak`, calculated using the `ratio_method`
#'
#' * `ratio_sem`: Standard error of the mean for the ratio
#'
#' * `number_of_scans`: Number of scans used for the final ratio calculation
#'
#' * `minutes_to_1e6_ions`: Time in minutes it would take to observe 1 million ions of the `isotopocule` used as numerator of the ratio calculation.
#'
#' * `shot_noise_permil`: Estimate of the shot noise (more correctly thermal noise) of the reported ratio in permil.
#'
#' * `ratio_relative_sem_permil`: Relative standard error of the reported ratio in permil
#'
#' @export
orbi_summarize_results <- function(
    dataset,
    ratio_method = c("mean", "sum", "median", "geometric_mean", "slope", "weighted_sum"),
    .by = c("block", "sample_name", "segment", "data_group", "data_type", "injection")) {

  # basic checks
  if (missing(dataset))
    stop("no input for dataset supplied", call. = TRUE)

  if (is.data.frame(dataset) == FALSE)
    stop("dataset must be a data frame",  call. = TRUE)

  if (missing(ratio_method))
    stop("no input for ratio_method supplied", call. = TRUE)

  # make sure the ratio method argument is valid
  ratio_method <- rlang::arg_match(ratio_method)

  # check that required data columns are present
  base_group_cols <- c("filename", "compound", "basepeak", "isotopocule")
  req_cols <- c(base_group_cols, "time.min", "ions.incremental", "basepeak_ions")
  if (length(missing_cols <- setdiff(req_cols, names(dataset))) > 0) {
    paste0("Missing required column(s): ",
           paste(missing_cols, collapse = ", ")) |>
      stop(call. = FALSE)
  }

  # set basic groupings (use .add so prior group_by groupings are also preserved)
  df.group <- dataset |> dplyr::group_by(!!!lapply(base_group_cols, rlang::sym), .add = TRUE)

  # add additional groupings
  add_groups <- c()
  if (!missing(.by)) {
    # user defined
    by_quo <- rlang::enquo(.by)
    add_groups <- tidyselect::eval_select(expr = by_quo, data = dataset) |> names()
  } else {
    # default .by - only use the columns that actually exist in the data
    add_groups <- intersect(.by, names(dataset))
  }
  if (length(add_groups) > 0) {
    df.group <- df.group |>
      dplyr::group_by(!!!lapply(add_groups, rlang::sym), .add = TRUE)
  }

  # info message
  start_time <-
    sprintf(
      "orbi_summarize_results() is grouping the data by %s and summarizing ratios using the '%s' method...",
      sprintf("'%s'", dplyr::group_vars(df.group)) |> paste(collapse = ", "),
      ratio_method
    ) |>
    message_start()

  # calculations
  df.stat <-
    try_catch_all({
      # run calculations
      df.group |>

        dplyr::summarize(

          # scan information
          start_scan.no = min(.data$scan.no),
          end_scan.no = max(.data$scan.no),

          # time information
          start_time.min = min(.data$time.min),
          mean_time.min = mean(.data$time.min),
          end_time.min = max(.data$time.min),

          # ratio calculation
          ratio = orbi_calculate_summarized_ratio(
            .data$ions.incremental,
            .data$basepeak_ions,
            ratio_method = !!ratio_method
          ),

          shot_noise_permil =
            1000 * (sqrt(
              (sum(.data$ions.incremental) + sum(.data$basepeak_ions)) /
                (sum(.data$ions.incremental) * sum(.data$basepeak_ions)))),

          ratio_sem = calculate_ratios_sem(
            ratios = .data$ions.incremental / .data$basepeak_ions),

          minutes_to_1e6_ions = (1E6 / sum(.data$ions.incremental)) *
            (max(.data$time.min) - min(.data$time.min)),

          number_of_scans = length(.data$ions.incremental / .data$basepeak_ions),

          .groups = "drop") |>

        dplyr::mutate(ratio_relative_sem_permil = 1000 * (.data$ratio_sem / .data$ratio))   |>

        # round values for output
        dplyr::mutate(
          ratio = round(.data$ratio, 8),
          ratio_sem = round(.data$ratio_sem, 8),
          ratio_relative_sem_permil = round(.data$ratio_relative_sem_permil, 3),
          shot_noise_permil = round(.data$shot_noise_permil, 3),
          minutes_to_1e6_ions = round(.data$minutes_to_1e6_ions, 2)
        )  |>
        # sort table by the grouping variables
        dplyr::arrange(!!!lapply(dplyr::group_vars(df.group), rlang::sym)) |>
        # rearrange column order
        dplyr::relocate("ratio_relative_sem_permil", .after = "ratio")

    },
    "something went wrong summarizing the results: "
    )

  # info
  message_finish("completed", start_time = start_time)

  # return
  return(df.stat)

}
