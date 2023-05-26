# Functions to calculate ratios and stats --------------------------------------------

#' @title Internal function to calculate standard error
#' @description The function `calculate_ratios_sem()` computes a regular standard error
#' @keywords internal
#' @param ratios A numeric vector used to calculate a standard error
#' @return The calculated standard error

calculate_ratios_sem <- function(ratios) {

  # safety checks
  if (missing(ratios))
    stop("no input vector for ratios supplied", call. = TRUE)

  # basic checks
  if (!(is.vector(ratios)))
    stop("ratios need to be provided in a vector", call. = TRUE)

  if (!(is.numeric(ratios)))
    stop("ratios need to be a numeric vector", call. = TRUE)

  if (length(ratios) <= 1)
    stop("length of ratios needs to be > 1: ", length(ratios), call. = TRUE)


  tryCatch(

    stats::sd(ratios) / sqrt(length(ratios)),
    warning = function(w) {
      stop("something went wrong calculating the standard error: ",
           w$message,
           call. = TRUE)
    }
  )

}

#' @title Internal function to calculate geometric mean
#' @description  The function `calculate_ratios_gmean()` is used to calculate geometric means
#' @keywords internal
#' @param ratios A numeric vector of ratios used to calculate the geometric mean
#' @return The calculated geometric mean

calculate_ratios_gmean <- function(ratios) {

  if (missing(ratios))
    stop("input vector for ratios supplied", call. = TRUE)

  # basic checks
  if (!(is.vector(ratios)))
    stop("ratios need to be provided in a vector", call. = TRUE)

  if (!(is.numeric(ratios)))
    stop("ratios need to be a numeric vector", call. = TRUE)

  if (length(ratios) <= 1)
    stop("length of ratios needs to be > 1: ", length(ratios), call. = TRUE)

  tryCatch(

    exp(mean(log(ratios))),
    warning = function(w) {
      stop("something went wrong calculating the geometic mean: ", w$message, call. = TRUE)
    }
  )
}


#' @title Internal function to calculate standard deviation (geometric)
#' @description  The function `calculate_ratios_gsd()` is used to calculate geometric standard deviations
#' @keywords internal
#' @param ratios A numeric values used to calculate the geometric standard deviation
#' @return The calculated geometric standard deviation
calculate_ratios_gsd <- function(ratios) {

  if (missing(ratios))
    stop("no input vector for ratios supplied", call. = TRUE)

  # basic checks
  if (!(is.vector(ratios)))
    stop("ratios need to be provided in a vector", call. = TRUE)

  if (!(is.numeric(ratios)))
    stop("ratios need to be a numeric vector", call. = TRUE)

  if (length(ratios) <= 1)
    stop("length of ratios needs to be > 1: ", length(ratios), call. = TRUE)

  tryCatch(

    exp(mean(log(ratios)) + stats::sd(log(ratios))) - exp(mean(log(ratios))),
    warning = function(w) {
      stop("something went wrong calculating geometric standard deviaton: ", w$message, call. = TRUE)
    }
  )

}

# @title Internal function to calculate standard error (geometric)
# @description  The function `calculate_ratios_gse()` is used to calculate geometric standard errors
# @keywords internal
# @param ratios A vector of values used to calculate geometric standard errors
# @return The calculated geometric standard error
calculate_ratios_gse <- function(ratios) {

  if (missing(ratios))
    stop("input vector for ratios supplied", call. = TRUE)

  # basic checks
  if (!(is.vector(ratios)))
    stop("ratios need to be provided in a vector", call. = TRUE)

  if (!(is.numeric(ratios)))
    stop("ratios need to be a numeric vector", call. = TRUE)

  if (length(ratios) <= 1)
    stop("length of ratios needs to be > 1: ", length(ratios), call. = TRUE)

  tryCatch(
    (exp(mean(log(ratios)) + stats::sd(log(ratios))) - exp(mean(log(ratios)))) / sqrt(length(ratios)),
    warning = function(w) {
      stop("something went wrong calculating the geometric standard error: ", w$message, call. = TRUE)
    }
  )

}

#' @title Internal function for ratio_method `slope`
#' @description  The function `calculate_ratios_slope()` is used to estimate the slope of x, y values used in a ratio
#' @keywords internal
#' @param x Vector of values used as ratio numerator
#' @param y Vector of values used as ratio denominator
#' @details The slope is calculated from a linear regression model that is weighted by the numerator x, using `stats::lm(x ~ y + 0, weights = x)`
#' @return The calculated slope, an estimate of the ratio x/y
calculate_ratios_slope <- function(x, y) {

  if (missing(x))
    stop("no input vector for x supplied", call. = TRUE)

  if (missing(y))
    stop("no input vector for y supplied", call. = TRUE)

  # basic checks
  if (!(is.vector(x)))
    stop("x needs to be a vector", call. = TRUE)

  if (!(is.numeric(x)))
    stop("x needs to be a numeric vector", call. = TRUE)

  if (length(x) <= 1)
    stop("length of x needs to be > 1: ", length(x), call. = TRUE)

  if (!(is.vector(y)))
    stop("y needs to be a vector", call. = TRUE)

  if (!(is.numeric(y)))
    stop("y needs to be a numeric vector", call. = TRUE)

  if (length(y) <=1)
    stop("length of y needs to be > 1: ", length(y), call. = TRUE)

  if (length(x) != length(y))
    stop("length of x and y need to be equal", call. = TRUE)

  tryCatch(

    # Note order of x and y to get correct slope!
    model <-
      stats::lm(x ~ y + 0, weights = x),

    warning = function(w) {
      stop("something went wrong calculating the ratio as slope using a linear model: ", w$message, call. = TRUE)
    }
  )

  sl <- model$coefficients[[1]]

  return(sl)

}

#' @title Internal function for ratio_method `weighted_sum`
#' @description The function `calculate_ratios_weighted_sum()` is used to calculate ratios by weighted sums of x and y values
#' @keywords internal
#' @param x A vector of values used as ratio numerator
#' @param y A vector of values used as ratio denominator
#' @details The weighing function ensures that each scan contributes equal weight to the ratio calculation,
#' i.e. scans with more ions in the Orbitrap do not contribute disproportionally to the total sum of x and y that is used to calculate x/y.
#' @return The calculated ratio x/y
calculate_ratios_weighted_sum <- function(x, y) {

  if (missing(x))
    stop("no input vector for x supplied", call. = TRUE)

  if (missing(y))
    stop("no input vector for y supplied", call. = TRUE)

  # basic checks
  if (!(is.vector(x)))
    stop("x needs to be a vector", call. = TRUE)

  if (!(is.numeric(x)))
    stop("x needs to be a numeric vector", call. = TRUE)

  if (length(x) <= 1)
    stop("length of x needs to be > 1: ", length(x), call. = TRUE)

  if (!(is.vector(y)))
    stop("y needs to be a vector", call. = TRUE)

  if (!(is.numeric(y)))
    stop("y needs to be a numeric vector", call. = TRUE)

  if (length(y) <= 1)
    stop("length of y needs to be > 1: ", length(y), call. = TRUE)

  if (length(x) != length(y))
    stop("length of x and y need to be equal", call. = TRUE)


  df <- cbind(x, y)

  avg.ions <- (sum(df[, 1]) + sum(df[, 2])) / length(df[, 1])

  scan.ions <- (df[, 1] + df[, 2])

  weighted.x <- avg.ions / scan.ions  * as.numeric(df[, 1])
  weighted.y <- avg.ions / scan.ions  * as.numeric(df[, 2])

  tryCatch(

    # Note order of x and y to get correct slope!

    ratio <-sum(weighted.x) / sum(weighted.y),


    warning = function(w) {
      stop("something went wrong calculating the ratio from weighted sums: ", w$message, call. = TRUE)
    }
  )

  return(ratio)
}


#' @title Calculate isotopocule ratios
#' @description This function calculates the ratio of two isotopocules (the `numerator` and `denominator`) by averaging multiple measurements of each using the `ratio_method` and returns a single value. Normally this function is not called directly by the user, but via the function [orbi_summarize_results()], which calculates isotopocule ratios and other results for an entire dataset.
#'
#' @param numerator Column(s) used as numerator; contains ion counts
#' @param denominator Column used as denominator; contains ion counts
#' @param ratio_method Method for computing the ratio. **Please note well**: the formula used to calculate ion ratios matters! Do not simply use arithmetic mean. The best option may depend on the type of data you are processing (e.g., MS1 versus M+1 fragmentation). `ratio_method` can be one of the following:
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
#' fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
#' df <-
#'   orbi_read_isox(file = fpath) |>
#'   orbi_simplify_isox() |>
#'   orbi_define_basepeak(basepeak_def = "M0")
#'
#' ratio <- orbi_calculate_ratios(
#'    numerator = df$ions.incremental,
#'    denominator = df$basepeak_ions,
#'    ratio_method = "sum")
#'
#' @return Single value ratio between the isotopocules defined as `numerator` and `denominator` calculated using the `ratio_method`.
#'
#' @export
orbi_calculate_ratios <- function(
    numerator, denominator,
    ratio_method = c("mean", "sum", "median", "geometric_mean", "slope", "weighted_sum")) {

  if (missing(numerator))
    stop("no input for numerator supplied", call. = TRUE)

  if (is.numeric(numerator) == FALSE)
    stop("numerator must be a numeric vector",  call. = TRUE)

  if (missing(denominator))
    stop("no input for denominator supplied", call. = TRUE)

  if (is.numeric(denominator) == FALSE)
    stop("denominator must be a numeric vector",  call. = TRUE)


  tryCatch({ o <-  {

    if (ratio_method == "mean") {
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
      stop(
        "`ratio_method` has to be `mean`, `sum`, `median`, `geometric_mean`, `slope` or `weighted_sum`",
        call. = FALSE
      )
    }
  }

   warning = function(w) {
     stop("something went wrong calculating ratios:", w$message, call. = TRUE)

  }
  })

  return(o)

}

#' @title Generate the results table
#' @description Contains the logic to generate the results table. It passes the  `ratio_method` parameter to the [orbi_calculate_ratios()] function for ratio calculations.
#' @param dataset A tibble from `IsoX` output ([orbi_read_isox()]) and with a basepeak already defined (using `orbi_define_basepeak()`). Optionally, with block definitions ([orbi_define_blocks_for_dual_inlet()]) or even additional block segments ([orbi_segment_blocks()]).
#' @inheritParams orbi_calculate_ratios
#' @param .by additional grouping columns for the results summary (akin to [dplyr's `.by` parameter][dplyr::summarize()]). If not set by the user, all columns in the parameter's default values are used, if present in the dataset. Note that the order of these is also used to arrange the summary.
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
#' df <- orbi_read_isox(file = fpath) |>
#'       orbi_simplify_isox() |> orbi_define_basepeak(basepeak_def = "M0")  |>
#'       orbi_summarize_results(ratio_method = "sum")
#'
#' @return Returns a results summary table retaining the columns `filename`, `compound`, `isotopocule` and `basepeak` as well as the grouping columns from the `.by` parameter that are part of the input `dataset`. Additionally this function adds the following results columns:  `ratio`, `ratio_sem`, `ratio_relative_sem_permil`, `shot_noise_permil`, `No.of.Scans`, `minutes_to_1e6_ions`
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
  sprintf(
    "orbi_summarize_results() is grouping the data by %s and summarizing ratios using the '%s' method...",
    sprintf("'%s'", dplyr::group_vars(df.group)) |> paste(collapse = ", "),
    ratio_method
  ) |>
    message()

  tryCatch(
    # run calculations
    df.stat <- df.group |>

      dplyr::summarize(
        ratio = orbi_calculate_ratios(
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
      dplyr::relocate("ratio_relative_sem_permil", .after = "ratio"),

    #For simplicity use basic standard error for all options

    warning = function(w) {
      stop("something went wrong summarizing the results: ",
           w$message,
           call. = TRUE)
    }
  )


  return(df.stat)

}
