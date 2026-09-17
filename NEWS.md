# isoorbi 1.6.0

## Breaking changes

 * the `isoraw` raw file reader now requires version 0.3 or later (`orbi_check_isoraw()` upgrades automatically). Version 0.3 no longer reports the `is_ref` and `is_lock_peak` columns and instead reports the raw Thermo `PeakOptions` bitmask in a `flags` column, from which these and other flags can be derived.
 * version 0.3 of the reader also reads **all** peaks by default, including the ones the centroider flagged as problematic (saturated, fragmented, merged, exception, modified). Previously these were silently discarded during the read. Pass `--skipProblematicPeaks` to the executable to get the old behavior, or filter them out after aggregation (see below).
 * as a result of the above, the `peaks` dataset of the aggregators now provides a readable `flags` column (a factor with values such as `"none"`, `"reference"` or `"exception + fragmented"`) instead of the previous `isRefPeak` and `isLockPeak` columns. Any cached raw files (`.raw.cache.zip`) created with an earlier version of the reader have to be re-read to pick up the new columns.

## New features

 * new peak flags function `orbi_peak_flags_include()` to work with the peak flags. To filter for an exact set of flags, compare the `flags` column directly (it is a factor, so this is fast), e.g. `dplyr::filter(peaks, flags == "none")` keeps only the peaks without any flags and `flags %in% c("lock mass", "reference")` only those that are exclusively either a reference peak or a lock mass peak. Use `orbi_peak_flags_include(flags, "reference")` to find every peak carrying the reference flag whether or not it carries others.

### Restoring the `isRefPeak` / `isLockPeak` columns

If you rely on the previous boolean columns, add them back with a custom aggregator:

```r
my_aggregator <- orbi_get_aggregator("standard") |>
  orbi_add_to_aggregator(
    "peaks", "isRefPeak", source = "flags",
    func = "orbi_peak_flags_include", args = list(flag = "reference"),
    cast = "as.logical"
  ) |>
  orbi_add_to_aggregator(
    "peaks", "isLockPeak", source = "flags",
    func = "orbi_peak_flags_include", args = list(flag = "lock peak"),
    cast = "as.logical"
  )

raw_files |> orbi_aggregate_raw(aggregator = my_aggregator)
```

Register it with `my_aggregator |> orbi_register_aggregator("my_aggregator")` to be able to refer to it by name in `orbi_aggregate_raw()`.

# isoorbi 1.5.3

This is a minor update to support the latest version of testthat and address a few small bugs.

## Bug fixes & improvements

 * updated snapshots for continuous integration tests to work with testthat 3.3.0
 * changed `orbi_export_data_to_excel()` to only export `file_info` and `summary` by default to avoid uintentionally enormous excel files when exporting large datasets (#106)
 * fixed issue with `orbi_define_basepeak()` not correctly handling unidentified peaks, they are now automatically filtered out when this function is called (#114)
 * improved error messages in functions that need an isotopocule defintion ifisotopocules have not yet been identified
 * clarified installation instructions

# isoorbi 1.5.2

This is a minor update to decrease package size for CRAN. 

## Enhancements

 * added `orbi_get_example_files()` that can download additional example files to provide easier access to test files for users

# isoorbi 1.5.1

This is a minor update to enhance usability.

## Enhancements

* show panel lables in spectral plots, i.e. M+1, M+2, etc. (#93)
* align x-axes in spectral plots (#98)
* introduce default_tolerance and default_charge parameters for `orbi_identify_isotopocules` (#95)
* automatically calculate ions during `orbi_identify_isotopocules`
* provide a stastic of ion counts identified when running `orbi_identify_isotopocules` (#94)
* provide more informative error when `orbi_read_raw` fails because the raw file is still being acquired
* provide `orbi_get_example_files` function as a one-point stop to get both system package test files as well as downloadable test files
* `orbi_find_raw` now finds both .raw and .RAW extensions (i.e. no longer case-sensitive)

# isoorbi 1.5.0

## Breaking changes

There are no breaking changes in this release (all changes and new features are backwards compatible).

## New features

* implemented an optimized and expanded raw file reader (isoraw) that is packaged  with isoorbi and is used in `orbi_read_raw()`
* implemented versatile isotopocule mapping via `orbi_identifiy_isotopocule()` from csv, tsv, and excel inputs
* implemented `orbi_calculate_ions()` to calculate ions from peak intensities and noise with flexible `CN` and `RN` parameters
* implemented `orbi_plot_spectra()` to plot 

## Enhancements

* upgraded all data processing and plotting functions to be compatible with both isox and raw file datasets
* improved default print outputs for S3 classes used in the package
* improved package structure documentation
* expanded `orbi_flag_outliers()` functionality (contributed by Florian Rubach)

## Bug fixes

* fixed incompatibility with ggplot2 version 4.0.0 (#69)

# isoorbi 1.4.0

## Breaking changes

There are no breaking changes in this release (all changes and new features are backwards compatible).

## New features

* implemented direct reading and caching of orbitrap IRMS raw files (`orbi_read_read()`) via the rawrr package
* use CLI for clearer and more informative info messages for all functions

## Enhancements

* renamed package "settings" to package "options" to be consistent with the naming conventions in base R (`orbi_options()` replaces `orbi_set_settings()` and `orbi_get_options()` / `orbi_get_option()` replaces `orbi_get_settings()`)
* improved functionality for options loading and defaults

# isoorbi 1.3.1

This is a patch containing compatibility updates.

# isoorbi 1.3.0

This is a minor release adding new features.

## Breaking changes

There are no breaking changes in this release (all changes and new features are backwards compatible).

## New features

Plotting functions:

* `orbi_plot_satellite_peaks()` implemented
* `orbi_plot_isotopocule_coverage()` implemented
* `orbi_plot_raw_data()` implemented

Information functions:

* `orbi_get_isotopocule_coverage()`

## Enhancements

* `orbi_simplify_isox()` now includes `intensity` by default and allows addition of user-specified columns
* `orbi_flag_satellite_peaks()`, `orbi_flag_weak_isotopocules()` and `orbi_flag_outliers()` now provide additional information when no data is flagged and how to visualize what was flagged (if anything)

## Bug Fixes

* `orbi_add_blocks_to_plot()` now supports log scale axes

# isoorbi 1.2.0

This is a minor release adding new features.

## Breaking changes

There are no breaking changes in this release (all changes and new features are backwards compatible).

## New features

* `orbi_find_isox()` added
* `orbi_analyze_noise()` implemented
* `orbi_plot_shot_noise()` implemented
* `orbi_flag_satellite_peaks()`, `orbi_flag_weak_isotopocules()` and `orbi_flag_outliers()` supersede `orbi_filter_satellite_peaks()`, `orbi_filter_weak_isotopocules()` and `orbi_filter_scan_intensity()`
* `orbi_export_data_to_excel()` implemented
* `orbi_define_block_for_flow_injection()` implemented
* `orbi_add_blocks_to_plot()` implemented

## Enhancements

* all loading and utility functions provide more details in the info message about what has been done, what was affected, and how long it took
* all loading and utility functions catch processing errors and report the information back to the user
* `orbi_read_isox()` now can read multiple .isox files at once and includes the .isox `filepath` in the resulting tibble

## Bug fixes

* data frame groupings introduced in flagging/filtering functions are now removed again at the end of the function

# isoorbi 1.1.0

This is a minor release adding new features and fixing a few bugs.

## Breaking changes

There are no breaking changes in this release (all changes and new features are backwards compatible).

## New features

* implemented functionality for dual inlet data acquisitions, see `orbi_define_blocks_for_dual_inlet()` and `orbi_get_blocks_info()`
* implemented package settings, using internal functions `isoorbi::orbi_get_settings`, `isoorbi::orbi_set_settings`

## Enhancements

* removed dependency on `stringr`
* implemented native pipe `|>` (R version requirement increased to 4.1.0) and removed dependency on `magrittr`
* added `.by` parameter for `orbi_summarize_results()` for option to manually adjust grouping

## Bug fixes

* `dplyr` changes to joins with explicit `multiple` argument. `multiple` now defaults to "all", and the options of "error" and "warning" are now deprecated in favor of using `relationship`. 

# isorbi 1.0.0

First public release.
