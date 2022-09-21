# Common utility functions

#' @title Remove rare isotopocules
#' @description Remove isotopocules that are not consistently detected across scans
#'
#' @param data Simplified IsoX data to be processed
#' @param min_percent Set threshold. Isotopocule must be observed in at least  x percent of scans
#'
#' @return Returns data frame with inconsistent isotopocules removed
#' @export
orbi_filter_rare <-
  function(data, min_percent) {
    remove.df <- data %>%
      dplyr::group_by(.data$filename) %>%
      dplyr::mutate(n.scans = max(.data$scan.no) - min(.data$scan.no)) %>% #FIXME: implement a more general solution here
      dplyr::group_by(.data$filename, .data$compound, .data$isotopolog) %>%
      dplyr::mutate(i.scans = length(.data$scan.no)) %>%
      dplyr::filter(.data$i.scans < min_percent / 100 * .data$n.scans) %>% # => update selection in GUI?, add message? used previously `input$rare`
      dplyr::select(-.data$n.scans, -.data$i.scans) %>% droplevels() %>% as.data.frame()


    df <-
      anti_join(
        data,
        remove.df,
        by = c(
          "filename",
          "scan.no",
          "time.min",
          "compound",
          "isotopolog",
          "ions.incremental",
          "tic",
          "it.ms"
        )
      ) #FIXME:More elegant/robust solution?

    return(df)

  }



#FIXME-LEIF: complete and confirm
#' @title remove.satellitePeaks
#' @description Remove minor signals (e.g., satellite peaks) that were reported by IsoX
#' @param df Simplified IsoX file to have satellites removed from
#' @return IsoX file with satellites removed
#' @export
orbi_filter_satellitePeaks <- function(df) {
  df <- df %>%
    dplyr::group_by(.data$filename, .data$compound, .data$scan.no, .data$isotopolog) %>%
    dplyr::filter(.data$ions.incremental == max(.data$ions.incremental))

  return(df)
}

#FIXME-LEIF: complete and confirm
#' @title Filter by TIC x Injection Time
#' @description Remove extreme scans based on TIC x Injection time
#' @param df Simplified IsoX file to have 'ticxit' removed
#' @param extreme_min ???
#' @return Returns filtered IsoX file based of input value
#' @export
orbi_filter_TICxIT <- function(df, extreme_min) {
  df <-
    df %>% dplyr::group_by(.data$filename) %>% dplyr::mutate(TICxIT = .data$tic * .data$it.ms) %>% #edit 28-Feb-2022 enable multiple dual inlet files
    dplyr::filter(
      .data$TICxIT > stats::quantile(.data$TICxIT, extreme_min / 100) &
        .data$TICxIT < stats::quantile(.data$TICxIT, 1 - extreme_min / 100)
    ) %>%
    dplyr::select(-.data$TICxIT)

  return(df)
}

#FIXME-LEIF: complete and confirm
#' @title orbi_filter_isox
#' @description A basic dplyr::filter(should be generalized for most use cases)
#' @param dataset The file.tsv to be filtered
#' @param isotopocules Vector of isotopocules to select
#' @param base_peak Name of base peak to select
#' @param isox_files Vector of isox files to select
#' @param compounds Vector of compounds files to select
#' @param time_min Minimum time in minutes
#' @param time_max Maximum time in minutes
#' @return Returns a filtered IsoX file
#' @export
orbi_filter_isox <- function(dataset, isotopocules, base_peak, isox_files, compounds, time_min, time_max) {
  df.out <- dataset %>%
    # isotopolog filter
    dplyr::filter(.data$isotopolog %in% c(isotopocules, base_peak)) %>% #maybe this line should be moved out of the core function!
    # file name filter
    {
      if (!"all" %in% isox_files)
        dplyr::filter(., .data$filename %in% isox_files)
      else
        .
    } %>%
    # compounds filter
    {
      if (!"all" %in% compounds)
        dplyr::filter(., .data$compound %in% compounds)
      else
        .
    } %>%
    # time range filter
    dplyr::filter(.data$time.min >= time_min, .data$time.min <= time_max)


  return(df.out)
}


# Functions to calculate stats --------------------------------------------

# isox data manipulation

#' @title Standard error
#' @description Calculates standard error
#' @param x Vector to compute standard error from
#' @return Standard error

orbi_calculate_se <- function(x) {
  stats::sd(x) / sqrt(length(x))
}

#' @title Geometric mean
#' @description  Define 'gmean' to calculate geometic mean
#'
#' @param x value to compute geometric mean from
#'
#' @return The calculated geometric mean

orbi_calculate_gmean <- function(x) {
  exp(mean(log(x)))
} #define geometric mean

#' @title Standard deviation (geometric)
#' @description  Define 'gsd' to calculate geometic standard deviation
#'
#' @param x value to compute geometric standard deviation from
#'
#' @return The calculated geometric standard deviation

orbi_calculate_gsd <- function(x) {
  exp(mean(log(x)) + stats::sd(log(x))) - exp(mean(log(x)))
}

#' @title Standard error (geometric)
#' @description  Define 'gse' to calculate geometic standard error
#'
#' @param x value to compute geometric standard error from
#'
#' @return The calculated geometric standard error

orbi_calculate_gse <- function(x) {
  (exp(mean(log(x)) + stats::sd(log(x))) - exp(mean(log(x)))) / sqrt(length(x))
}

#' @title slope
#' @description  Define 'slope' to calculate geometic mean
#'
#' @param x ???
#' @param y ???
#'
#' @return The slope

orbi_calculate_slope <- function(x, y) {
  model <-
    stats::lm(x ~ y + 0, weights = x) #Note order of x and y to get correct slope!
  sl <- model$coefficients[[1]]
  sl
}

#' @title weighted.vector.sum
#' @description ???
#'
#' @param x ???
#' @param y ???
#'
#' @return ???

orbi_calculate_weighted.vector.sum <- function(x, y) {
  df <- cbind(x, y)

  avg.ions <- (sum(df[, 1]) + sum(df[, 2])) / length(df[, 1])

  scan.ions <- (df[, 1] + df[, 2])

  weighted.x <- avg.ions / scan.ions  * as.numeric(df[, 1])
  weighted.y <- avg.ions / scan.ions  * as.numeric(df[, 2])
  ratio <- sum(weighted.x) / sum(weighted.y)
  ratio
}

#' @title Calculate ratio
#' @description Ratio calculation between peaks of interest. NOTE: The formula used to average ratios matters! Do not simply use arithmetic mean.
#'
#' @param peak1 Peak 1 of interest
#' @param peak2 Peak 2 of interest
#' @param ratio.method The desired method of computing ratio
#'
#' @return calculated ratio between peak1 and peak2 with selected ratio method

orbi_calculate_ratio <-
  function(peak1,
           peak2,
           ratio.method = c("mean",
                            "sum",
                            "median",
                            "geometric.mean",
                            "slope",
                            "weighted.vector.sum")) {
    if (ratio.method == "mean") {
      o <- base::mean(peak1 / peak2)
      o
    } else {
      if (ratio.method == "slope") {
        o <- orbi_calculate_slope(peak1, peak2)
        o
      } else{
        if (ratio.method == "sum") {
          o <-  base::sum(peak1) / sum(peak2)
          o
        } else{
          if (ratio.method == "geometric.mean") {
            o <- orbi_calculate_gmean(peak1 / peak2)
            o
          } else{
            if (ratio.method == "weighted.vector.sum") {
              o <- orbi_calculate_weighted.vector.sum(peak1, peak2)
              o
            } else{
              if (ratio.method == "median") {
                o <-
                  stats::median(peak1 / peak2)
                o
              } else{
                print(
                  "`ratio.method` has to be `mean`, `sum`, `median`, `geometric.mean`, `slope` or `weighted.vector.sum`"
                )
              }
            }
          }
        }
      }
    }
  }
