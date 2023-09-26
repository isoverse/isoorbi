
#' shot noise calculation
#'
#' will calculate for all combinations of `filename`, `compound`, and `isotopocule` in the provided `dataset`
#'
#' @title Shot noise calculation
#' @description This function computes the shot noise calculation
#' @param dataset a data frame output after running `orbi_define_basepeak()`
#' @return The processed data frame with new columns: `n_isotopocule_ions`, `n_effective_ions`, `ratio`, `ratio_rel_se.permil`, `shot_noise.permil`
#' @export
orbi_calculate_shot_noise <- function(dataset){

  # safety checks
  stopifnot(
    "need a `dataset` data frame" = !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filename`, `compound` and `isotopocule`" = all(c("filename", "compound", "isotopocule") %in% names(dataset)),
    "`dataset` requires defined basepeak (column `basepeak_ions`), make sure to run `orbi_define_basepeak()` first" = "basepeak_ions" %in% names(dataset)
  )

  # info message
  sprintf(
    "orbi_calculate_shot_noise() is calculating shot noise for %d measurements across %d file(s) with %d compound(s) and %d isotopocule(s)...",
    nrow(dataset), length(unique(dataset$filename)), length(unique(dataset$compound)), length(unique(dataset$isotopocule))
  ) |> message()

  # calculation
  dataset |>
    # preserve original row order
    dplyr::mutate(..idx = dplyr::row_number()) |>
    # make sure order is compatible with cumsum based calculations
    dplyr::arrange(.data$filename, .data$compound, .data$isotopocule, .data$scan.no) |>
    dplyr::mutate(
      # group by filename, compound, isotopocule.
      .by = c("filename", "compound", "isotopocule"),
      # cumulative ions
      n_isotopocule_ions = cumsum(ions.incremental),
      n_basepeak_ions = cumsum(basepeak_ions),
      n_effective_ions = n_basepeak_ions * n_isotopocule_ions / (n_basepeak_ions + n_isotopocule_ions),
      # relative standard error
      n_measurements = 1:n(),
      ratio = ions.incremental / basepeak_ions,
      ratio_mean = cumsum(ratio) / n_measurements,
      ratio_gmean = exp(cumsum(log(ratio)) / n_measurements),
      #ratio_sd = sqrt( cumsum( (ratio - ratio_mean)^2 ) / (n_measurements - 1L) ),
      # alternative std. dev. formula compatible with cumsum
      # note that adjustment for sample sdev --> sqrt(n/(n-1)) partially cancels with 1/sqrt(n) for SD to SE
      ratio_se = sqrt(cumsum(ratio^2) / n_measurements - ratio_mean^2) * sqrt(1/(n_measurements - 1L)),
      # Q: do you really want to use gmean here but not in the std. dev calculation?
      ratio_rel_se.permil = 1000 * ratio_se / ratio_gmean,
      # shot noise - calc is same as 1000 * (1 / n_basepeak_ions + 1 / n_isotopocule_ions) ^ 0.5 but faster
      shot_noise.permil = 1000 * n_effective_ions ^ -0.5
    ) |>
    # restor original row order
    dplyr::arrange(..idx) |>
    # remove columns not usually used downstream
    select(-"..idx", -"n_basepeak_ions", -"n_measurements", -"ratio_mean", -"ratio_gmean", -"ratio_se")
}

