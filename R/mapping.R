# peak mapping for isotopulues

#' Identify isotopocules
#'
#' Map the mass spectral peaks to specific isotopocules based on their mass.
#'
#' @param aggregated_data either data aggregated from [orbi_aggregate_raw()] or a straight-up tibble data frame of the peaks (e.g. retrieved via `orbi_get_data(peaks = everything())`).
#' @param isotopocules list of isotopocules to map, can be a data frame/tibble or name of a file to read from (.csv/.tsv/.xlsx are all supported).
#' @return same object as provided in `aggregated_data` with an added column `isotopocule`
#' @export
orbi_identify_isotopocules <- function(aggregated_data, isotopocules) {
  # safety checks
  check_arg(
    aggregated_data,
    !missing(aggregated_data) &&
      (is(aggregated_data, "orbi_aggregated_data") ||
        is.data.frame(aggregated_data)),
    "must be a set of aggregated raw files or a data frame of peaks"
  )

  # keep track for later
  is_agg <- is(aggregated_data, "orbi_aggregated_data")
  peaks <- if (is_agg) aggregated_data$peaks else aggregated_data

  # FIXME: continue here - check the tibble for having the needed columns
}
