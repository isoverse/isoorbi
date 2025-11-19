# Filter out flagged data

This function filters out data that have been previously flagged using
functions
[`orbi_flag_satellite_peaks()`](https://isoorbi.isoverse.org/reference/orbi_flag_satellite_peaks.md),
[`orbi_flag_weak_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_flag_weak_isotopocules.md),
and/or
[`orbi_flag_outliers()`](https://isoorbi.isoverse.org/reference/orbi_flag_outliers.md).
Note that this function is no longer necessary to call explicitly as
[`orbi_analyze_shot_noise()`](https://isoorbi.isoverse.org/reference/orbi_analyze_shot_noise.md)
and
[`orbi_summarize_results()`](https://isoorbi.isoverse.org/reference/orbi_summarize_results.md)
automatically exclude flagged data.

## Usage

``` r
orbi_filter_flagged_data(dataset)
```

## Arguments

- dataset:

  a tibble with previously flagged data from
  [`orbi_flag_satellite_peaks()`](https://isoorbi.isoverse.org/reference/orbi_flag_satellite_peaks.md),
  [`orbi_flag_weak_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_flag_weak_isotopocules.md),
  and/or
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/reference/orbi_flag_outliers.md)

## Value

a dataset with the flagged data filtered out
