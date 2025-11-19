# Flag outlier scans

This function flags outliers using one of the different methods provided
by the parameters (to use multiple, please call this function several
times sequentially). Note that this function evaluates outliers within
each "uidx", "filename", and "injection" (for those of the columns that
exist), and additionally within each "block" and "segment" if
`by_block = TRUE`. in addition to any groupings already defined before
calling this function using dplyr's `group_by()`. It restores the
original groupings in the returned datasert.

## Usage

``` r
orbi_flag_outliers(
  dataset,
  agc_fold_cutoff = NA_real_,
  agc_window = c(),
  agc_absolute_cutoff = c(),
  by_block = TRUE
)
```

## Arguments

- dataset:

  An aggregated dataset or a data frame of peaks (i.e. works directly
  after
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
  as well as with a tibble from [orbi_get_data(peaks =
  everything())](https://isoorbi.isoverse.org/reference/orbi_get_data.md)
  or when reading from an IsoX file)

- agc_fold_cutoff:

  flags scans with a fold cutoff based on the average number of ions in
  the Orbitrap analyzer. For example, `agc_fold_cutoff = 2` flags scans
  that have more than 2 times, or less than 1/2 times the average. TIC
  multiplied by injection time serves as an estimate for the number of
  ions in the Orbitrap.

- agc_window:

  flags scans with a critically low or high number of ions in the
  Orbitrap analyzer. Provide a vector with 2 numbers `c(x,y)` flagging
  the lowest x percent and highest y percent. TIC multiplied by
  injection time serves as an estimate for the number of ions in the
  Orbitrap.

- agc_absolute_cutoff:

  flags scans with a number of ions in the Orbitrap analyzer outside of
  an absolute range. Provide a vector with 2 numbers `c(x,y)` flagging
  data below x and above y of the TIC multiplied by injection time
  (which serves as an estimate for the number of ions in the Orbitrap).

- by_block:

  if the `dataset` has block and segment definitions, should the outlier
  flag be evaluated within each block+segment or globally? default is
  within each block+segment, switch to globally by turning
  `by_block = FALSE`

## Value

same object as provided in `dataset` with new columns `is_outlier` and
`outlier_type` (if they don't already exist) that flags outliers
identified by the method and provides the type of outlier (e.g. "2 fold
agc cutoff"), respectively.

## Examples

``` r
fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
df <-
  orbi_read_isox(file = fpath) |>
  orbi_simplify_isox() |>
  orbi_flag_outliers(agc_window = c(1,99))
#> ✔ [21ms] orbi_read_isox() loaded 6449 peaks for 1 compound (HSO4-) with 5
#> isotopocules (M0, 33S, 17O, 34S, and 18O) from testfile_flow.isox
#> ✔ [5ms] orbi_simplify_isox() kept columns filepath, filename, scan.no,
#> time.min, compound, isotopocule, ions.incremental, tic, and it.ms
#> ✔ [27ms] orbi_flag_outliers() flagged 30/1290 scans (2.3%) as outliers based on
#> AGC window (1% to 99%) cutoff, i.e. based on scans whose number of ions tic *
#> it.ms in the Orbitrap analyzer fall into the lowest (<1%) or highest (>99%)
#> quantiles, in 3 data groups (based on filename) → use orbi_plot_raw_data(y =
#> tic * it.ms) to visualize them
```
