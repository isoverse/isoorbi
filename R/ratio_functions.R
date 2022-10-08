# Functions to calculate ratios and stats --------------------------------------------

#' @title Internal function to calculate standard error
#' @description The function `calculate_ratios_sem()` computes a regular standard error
#' @keywords internal
#' @param ratios A numeric vector used to calculate a standard error
#' @return The calculated standard error

calculate_ratios_sem <- function(ratios) {

  # safety checks
  if (missing(ratios))
    stop("input vector for ratios supplied", call. = TRUE)

  #basic checks
  if (!(is.vector(ratios)))
    stop("ratios needs to be a vector", call. = TRUE)
  if (!(is.numeric(ratios)))
    stop("ratios needs to be a numeric vector", call. = TRUE)

  if (length(ratios) <=1)
    stop("Length of ratios needs to be > 1: ", length(ratios), call. = TRUE)


  tryCatch(
    stats::sd(ratios) / sqrt(length(ratios)),
    warning = function(w) {
      stop("something went wrong calculating the standard error: ", w$message, call. = TRUE)
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

  #basic checks
  if (!(is.vector(ratios)))
    stop("ratios needs to be a vector", call. = TRUE)
  if (!(is.numeric(ratios)))
    stop("ratios needs to be a numeric vector", call. = TRUE)

  if (length(ratios) <=1)
    stop("Length of ratios needs to be > 1: ", length(ratios), call. = TRUE)

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
    stop("input vector for ratios supplied", call. = TRUE)

  #basic checks
  if (!(is.vector(ratios)))
    stop("ratios needs to be a vector", call. = TRUE)
  if (!(is.numeric(ratios)))
    stop("ratios needs to be a numeric vector", call. = TRUE)

  if (length(ratios) <=1)
    stop("Length of ratios needs to be > 1: ", length(ratios), call. = TRUE)

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

  #basic checks
  if (!(is.vector(ratios)))
    stop("ratios needs to be a vector", call. = TRUE)
  if (!(is.numeric(ratios)))
    stop("ratios needs to be a numeric vector", call. = TRUE)

  if (length(ratios) <=1)
    stop("Length of ratios needs to be > 1: ", length(ratios), call. = TRUE)

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
    stop("input vector for x supplied", call. = TRUE)

  if (missing(y))
    stop("input vector for y supplied", call. = TRUE)

  #basic checks
  if (!(is.vector(x)))
    stop("x needs to be a vector", call. = TRUE)
  if (!(is.numeric(x)))
    stop("x needs to be a numeric vector", call. = TRUE)

  if (length(x) <=1)
    stop("Length of x needs to be > 1: ", length(x), call. = TRUE)

  #basic checks
  if (!(is.vector(y)))
    stop("y needs to be a vector", call. = TRUE)
  if (!(is.numeric(y)))
    stop("y needs to be a numeric vector", call. = TRUE)

  if (length(y) <=1)
    stop("Length of y needs to be > 1: ", length(x), call. = TRUE)

  if (length(x) != length(y))
    stop("Length of x and y need to be equal", call. = TRUE)

  tryCatch(
    model <- stats::lm(x ~ y + 0, weights = x), #Note order of x and y to get correct slope!

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
    stop("input vector for x supplied", call. = TRUE)

  if (missing(y))
    stop("input vector for y supplied", call. = TRUE)

  #basic checks
  if (!(is.vector(x)))
    stop("x needs to be a vector", call. = TRUE)
  if (!(is.numeric(x)))
    stop("x needs to be a numeric vector", call. = TRUE)

  if (length(x) <=1)
    stop("Length of x needs to be > 1: ", length(x), call. = TRUE)

  #basic checks
  if (!(is.vector(y)))
    stop("y needs to be a vector", call. = TRUE)
  if (!(is.numeric(y)))
    stop("y needs to be a numeric vector", call. = TRUE)

  if (length(y) <=1)
    stop("Length of x needs to be > 1: ", length(x), call. = TRUE)

  if (length(x) != length(y))
    stop("Length of x and y need to be equal", call. = TRUE)


  df <- cbind(x, y)

  avg.ions <- (sum(df[, 1]) + sum(df[, 2])) / length(df[, 1])

  scan.ions <- (df[, 1] + df[, 2])

  weighted.x <- avg.ions / scan.ions  * as.numeric(df[, 1])
  weighted.y <- avg.ions / scan.ions  * as.numeric(df[, 2])

  tryCatch(
    ratio <- sum(weighted.x) / sum(weighted.y), #Note order of x and y to get correct slope!

    warning = function(w) {
      stop("something went wrong calculating the ratio from weighted sums: ", w$message, call. = TRUE)
    }
  )

  return(ratio)
}


#' @title Calculate isotopocule ratios
#' @description Ratio calculation between isotopocules and base peak defined by \code{\link{orbi_define_basepeak}}. Normally this function is not called directly by the user, but via the function \code{\link{orbi_summarize_results}}
#'
#' Please note well: The formula used to calculate ion ratios matters! Do not simply use arithmetic mean.
#' The best option may depend on the type of data you are processing (e.g., MS1 versus M+1 fragmentation).
#'
#'
#' @param numerator Column(s) used as numerator; contains ion counts
#' @param denominator Column used as denominator; contains ion counts
#' @param ratio_method Method for computing the ratio
#'
#'
#' @details **Description of options for `ratio_method`:**
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
#' df <- orbi_read_isox(filepath = fpath) %>%
#'                      orbi_simplify_isox() %>%
#'                      orbi_define_basepeak(basepeak_def = "M0")
#' ratios <- orbi_calculate_ratios(numerator = df$ions.incremental,
#'                           denominator = df$basepeak_ions,
#'                          ratio_method =  "sum")
#'
#' @return Calculated ratio between isotopocules defined as numerator(s) and denominator, using one of the ratio methods.
#' @export

orbi_calculate_ratios <- function(numerator,
                                 denominator,
                                 ratio_method = c("mean",
                                                  "sum",
                                                  "median",
                                                  "geometric_mean",
                                                  "slope",
                                                  "weighted_sum")) {


  if (missing(numerator))
    stop("no input for numerator supplied", call. = TRUE)

  if (is.numeric(numerator) == FALSE)
    stop("numerator must be a numeric vector",  call. = TRUE)

  if (missing(denominator))
    stop(" no input for denominator supplied", call. = TRUE)

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
       calculate_ratios_gmean(ratios= numerator / denominator)
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
#' @description Contains the logic to generate the results table. It passes the  \code{ratio_method} parameter to the \code{\link{orbi_calculate_ratios}} function for ratio calculations.
#' @param dataset A processed tibble produced from `IsoX` output
#' @inheritParams orbi_calculate_ratios
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
#' df <- orbi_read_isox(filepath = fpath) %>%
#'       orbi_simplify_isox() %>% orbi_define_basepeak(basepeak_def = "M0")  %>%
#'       orbi_summarize_results(ratio_method = "sum")
#'
#' @details **Description of the output columns:**
#'
#' * `basepeak`: Isotopocule used as denominator in ratio calculation.
#'
#' * `isotopocule`: Isotopocule used as numerator in ratio calculation.
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
#'
#' @return Returns a results table containing `filename`, `compound`,  `basepeak`, `Isotopocule`, `ratio`, `ratio_sem`, `ratio_relative_sem_permil`, `shot_noise_permil`, `No.of.Scans`, `minutes_to_1e6_ions`
#' @export
orbi_summarize_results <- function(dataset, ratio_method) {

  # basic checks
  if (missing(dataset))
    stop("no input for dataset supplied", call. = TRUE)

  if (is.data.frame(dataset) == FALSE)
    stop("dataset must be a data frame",  call. = TRUE)

  if (missing(ratio_method))
    stop("no input for ratio_method supplied", call. = TRUE)

  ratio.options <- c("mean",
                     "sum",
                     "median",
                     "geometric_mean",
                     "slope",
                     "weighted_sum")

  if (!(ratio_method %in% ratio.options))
    stop(cat(
      "ratio_method must be on of the following: ",
      ratio.options,
      "\n",
      sep = " "
    ),
    call. = TRUE)


  # check that requires columns are present
  req_cols <-
    c(
      "filename",
      "compound",
      "scan.no",
      "time.min",
      "isotopocule",
      "ions.incremental",
      "basepeak"
    )

  missing_cols <- setdiff(req_cols, names(dataset))

  if (length(missing_cols) > 0) {
    paste0("Missing required column(s): ",
           paste(missing_cols, collapse = ", ")) %>%
      stop(call. = FALSE)
  }


  # determine groupings

  all_groups <- c("filename", "compound", "basepeak", "isotopocule")
  if ("block" %in% names(dataset))
    all_groups <- c(all_groups, "block")
  if ("segment" %in% names(dataset))
    all_groups <- c(all_groups, "segment")

  sprintf("orbi_summarize_results() is grouping the data by %s and summarizing ratios using the '%s' method...",
          paste(all_groups, collapse = ", "), ratio_method) %>%
    message()

  # execute grouping
  df.group <- dataset %>%
    dplyr::group_by(!!!lapply(all_groups, rlang::sym))


  tryCatch(

    # run calculations
    df.stat <- df.group %>%
      summarize(
        ratio = orbi_calculate_ratios(.data$ions.incremental, .data$basepeak_ions, ratio_method = ratio_method),
        shot_noise_permil = 1000 * (sqrt((sum(.data$ions.incremental) + sum(.data$basepeak_ions)) / (sum(.data$ions.incremental) * sum(.data$basepeak_ions)))),
        ratio_sem = calculate_ratios_sem(ratios = .data$ions.incremental / .data$basepeak_ions),
        minutes_to_1e6_ions = (1E6 / sum(.data$ions.incremental)) * (max(.data$time.min) - min(.data$time.min)),
        number_of_scans = length(.data$ions.incremental / .data$basepeak_ions),
        .groups = "drop") %>%
      mutate(ratio_relative_sem_permil = 1000 * (.data$ratio_sem / .data$ratio))   %>%

      #Round values for output
    dplyr::mutate(
      ratio = round(.data$ratio, 8),
      ratio_sem = round(.data$ratio_sem, 8),
      ratio_relative_sem_permil = round(.data$ratio_relative_sem_permil, 3),
      shot_noise_permil = round(.data$shot_noise_permil, 3),
      minutes_to_1e6_ions = round(.data$minutes_to_1e6_ions, 2)
    )  %>%
      dplyr::arrange(.data$filename, .data$compound, .data$isotopocule) %>%
      dplyr::relocate(.data$ratio_relative_sem_permil, .after = .data$ratio),

    #For simplicity use basic standard error for all options

    warning = function(w) {
      stop("something went wrong summarizinf the results: ",
           w$message,
           call. = TRUE)
    }
  )


  df.stat <- as.data.frame(df.stat)
  return(df.stat)
}
