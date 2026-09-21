# Changelog

## isoorbi 1.6.0

This release switches to version 0.3 of the `isoraw` raw file reader,
which changes how peak flags are reported and which peaks are read by
default.

### Breaking changes

- the `isoraw` raw file reader now requires version 0.3.0 or later
  ([`orbi_check_isoraw()`](https://isoorbi.isoverse.org/dev/reference/orbi_check_isoraw.md)
  upgrades automatically). Version 0.3 no longer reports the `is_ref`
  and `is_lock_peak` columns and instead reports the raw Thermo
  `PeakOptions` bitmask in a `flags` column, from which these and every
  other flag can be derived.
- version 0.3 of the reader also reads **all** peaks by default,
  including the ones the centroider flagged as problematic (saturated,
  fragmented, merged, exception, modified). Previously these were
  silently discarded during the read, so expect noticeably more peaks
  than before - for the example file bundled with the package the peak
  count goes from 126 to 307. Filter them out after aggregation (see
  below) if you do not want them.
- as a result of the above, the `peaks` dataset of the aggregators now
  provides a readable `centroiderFlags` column (a factor with values
  such as `"none"`, `"reference"` or `"exception + fragmented"`) instead
  of the previous `isRefPeak` and `isLockPeak` columns. The name makes
  clear that these flags come from the instrument’s centroider, as
  opposed to the peaks `isoorbi` itself flags later on (satellite peaks,
  weak isotopocules and outliers).
- [`orbi_define_block_for_flow_injection()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_block_for_flow_injection.md)
  is deprecated in favor of the new
  [`orbi_define_blocks()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_blocks.md),
  which is not specific to flow injection and can define several blocks
  at once. The old function still works, warns, and forwards to the new
  one.
- the `sample_name` column created by the block definition functions
  ([`orbi_define_blocks_for_dual_inlet()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_blocks_for_dual_inlet.md),
  [`orbi_define_block_for_flow_injection()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_block_for_flow_injection.md)
  and
  [`orbi_adjust_block()`](https://isoorbi.isoverse.org/dev/reference/orbi_adjust_block.md))
  is now called `block_name` to reflect that it names the block rather
  than necessarily a sample. The `sample_name` argument of
  [`orbi_define_block_for_flow_injection()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_block_for_flow_injection.md)
  is renamed to `block_name` accordingly - the old argument still works
  but is deprecated and warns - and
  [`orbi_summarize_results()`](https://isoorbi.isoverse.org/dev/reference/orbi_summarize_results.md)
  groups by `block_name` instead of `sample_name` by default.
- raw file caches (`.raw.cache.zip`) created with an earlier version of
  the reader are detected and the corresponding raw file is read anew
  (with a warning). If the original `.raw` file is no longer available
  next to such a cache, the read reports a `cannot find this .raw file`
  problem (see
  [`orbi_get_problems()`](https://isoorbi.isoverse.org/dev/reference/problems.md)) -
  copy the `.raw` file back in or obtain an up to date cache.

### New features

- new
  [`orbi_peak_flags_include()`](https://isoorbi.isoverse.org/dev/reference/orbi_peak_flags.md)
  function works with the peak flags. To filter for an exact set of
  flags, compare the `centroiderFlags` column directly (it is a factor,
  so this is fast),
  e.g. `dplyr::filter(peaks, centroiderFlags == "none")` keeps only the
  peaks without any flags and
  `centroiderFlags %in% c("lock peak", "reference")` only those that are
  exclusively either a lock mass or a reference peak. Use
  `orbi_peak_flags_include(centroiderFlags, "reference")` on that column
  to find every peak carrying the reference flag whether or not it
  carries others.
- new
  [`orbi_define_blocks()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_blocks.md)
  that replaces
  [`orbi_define_block_for_flow_injection()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_block_for_flow_injection.md)
  and can define multiple blocks in one call. The block boundaries can
  be given as vectors
  (`orbi_define_blocks(start_time.min = c(0.1, 0.5), end_time.min = c(0.4, 0.8), block_name = c("first", "second"))`)
  or as a `blocks_table` data frame with any of the `start_time.min`,
  `end_time.min`, `start_scan.no`, `end_scan.no` and `block_name`
  columns. Each block is defined either by time or by scan number (they
  can be mixed between blocks) and the resulting blocks are listed in a
  summary message.
- [`orbi_check_isoraw()`](https://isoorbi.isoverse.org/dev/reference/orbi_check_isoraw.md)
  now confirms which reader version is ready for use instead of staying
  silent when nothing needs to be installed. Set `show_version = FALSE`
  to suppress that message (the automatic checks during a raw file read
  already do).

#### Restoring the `isRefPeak` / `isLockPeak` columns

If you rely on the previous boolean columns, add them back with a custom
aggregator:

``` r

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

Register it with
`my_aggregator |> orbi_register_aggregator("my_aggregator")` to be able
to refer to it by name in
[`orbi_aggregate_raw()`](https://isoorbi.isoverse.org/dev/reference/orbi_aggregate_raw.md).

### Bug fixes & improvements

- fixed the order of the legends in
  [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/dev/reference/orbi_add_blocks_to_plot.md)
  and
  [`orbi_plot_shot_noise()`](https://isoorbi.isoverse.org/dev/reference/orbi_plot_shot_noise.md).
  Without an explicit order ggplot2 does not guarantee a stable
  sequence, so the same plot could come out with its legends swapped on
  different operating systems or ggplot2 versions.
- fixed
  [`orbi_plot_spectra()`](https://isoorbi.isoverse.org/dev/reference/orbi_plot_spectra.md)
  including lock mass peaks with a missing intensity when
  `show_ref_and_lock_peaks = TRUE` (an operator precedence issue in the
  peak selection).
- documentation is now generated with roxygen2 8.0.0.
- deprecation warnings now show on every call to a deprecated function
  or argument instead of only once every 8 hours (this requires
  `lifecycle` 1.0.2 or later).

## isoorbi 1.5.3

This is a minor update to support the latest version of testthat and
address a few small bugs.

### Bug fixes & improvements

- updated snapshots for continuous integration tests to work with
  testthat 3.3.0
- changed
  [`orbi_export_data_to_excel()`](https://isoorbi.isoverse.org/dev/reference/orbi_export_data_to_excel.md)
  to only export `file_info` and `summary` by default to avoid
  uintentionally enormous excel files when exporting large datasets
  ([\#106](https://github.com/isoverse/isoorbi/issues/106))
- fixed issue with
  [`orbi_define_basepeak()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_basepeak.md)
  not correctly handling unidentified peaks, they are now automatically
  filtered out when this function is called
  ([\#114](https://github.com/isoverse/isoorbi/issues/114))
- improved error messages in functions that need an isotopocule
  defintion ifisotopocules have not yet been identified
- clarified installation instructions

## isoorbi 1.5.2

CRAN release: 2025-10-03

This is a minor update to decrease package size for CRAN.

### Enhancements

- added
  [`orbi_get_example_files()`](https://isoorbi.isoverse.org/dev/reference/orbi_get_example_files.md)
  that can download additional example files to provide easier access to
  test files for users

## isoorbi 1.5.1

This is a minor update to enhance usability.

### Enhancements

- show panel lables in spectral plots, i.e. M+1, M+2, etc.
  ([\#93](https://github.com/isoverse/isoorbi/issues/93))
- align x-axes in spectral plots
  ([\#98](https://github.com/isoverse/isoorbi/issues/98))
- introduce default_tolerance and default_charge parameters for
  `orbi_identify_isotopocules`
  ([\#95](https://github.com/isoverse/isoorbi/issues/95))
- automatically calculate ions during `orbi_identify_isotopocules`
- provide a stastic of ion counts identified when running
  `orbi_identify_isotopocules`
  ([\#94](https://github.com/isoverse/isoorbi/issues/94))
- provide more informative error when `orbi_read_raw` fails because the
  raw file is still being acquired
- provide `orbi_get_example_files` function as a one-point stop to get
  both system package test files as well as downloadable test files
- `orbi_find_raw` now finds both .raw and .RAW extensions (i.e. no
  longer case-sensitive)

## isoorbi 1.5.0

### Breaking changes

There are no breaking changes in this release (all changes and new
features are backwards compatible).

### New features

- implemented an optimized and expanded raw file reader (isoraw) that is
  packaged with isoorbi and is used in
  [`orbi_read_raw()`](https://isoorbi.isoverse.org/dev/reference/orbi_read_raw.md)
- implemented versatile isotopocule mapping via
  `orbi_identifiy_isotopocule()` from csv, tsv, and excel inputs
- implemented
  [`orbi_calculate_ions()`](https://isoorbi.isoverse.org/dev/reference/orbi_calculate_ions.md)
  to calculate ions from peak intensities and noise with flexible `CN`
  and `RN` parameters
- implemented
  [`orbi_plot_spectra()`](https://isoorbi.isoverse.org/dev/reference/orbi_plot_spectra.md)
  to plot

### Enhancements

- upgraded all data processing and plotting functions to be compatible
  with both isox and raw file datasets
- improved default print outputs for S3 classes used in the package
- improved package structure documentation
- expanded
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/dev/reference/orbi_flag_outliers.md)
  functionality (contributed by Florian Rubach)

### Bug fixes

- fixed incompatibility with ggplot2 version 4.0.0
  ([\#69](https://github.com/isoverse/isoorbi/issues/69))

## isoorbi 1.4.0

### Breaking changes

There are no breaking changes in this release (all changes and new
features are backwards compatible).

### New features

- implemented direct reading and caching of orbitrap IRMS raw files
  (`orbi_read_read()`) via the rawrr package
- use CLI for clearer and more informative info messages for all
  functions

### Enhancements

- renamed package “settings” to package “options” to be consistent with
  the naming conventions in base R
  ([`orbi_options()`](https://isoorbi.isoverse.org/dev/reference/orbi_options.md)
  replaces
  [`orbi_set_settings()`](https://isoorbi.isoverse.org/dev/reference/orbi_set_settings.md)
  and
  [`orbi_get_options()`](https://isoorbi.isoverse.org/dev/reference/orbi_options.md)
  /
  [`orbi_get_option()`](https://isoorbi.isoverse.org/dev/reference/orbi_options.md)
  replaces
  [`orbi_get_settings()`](https://isoorbi.isoverse.org/dev/reference/orbi_get_settings.md))
- improved functionality for options loading and defaults

## isoorbi 1.3.1

CRAN release: 2024-08-27

This is a patch containing compatibility updates.

## isoorbi 1.3.0

CRAN release: 2023-11-09

This is a minor release adding new features.

### Breaking changes

There are no breaking changes in this release (all changes and new
features are backwards compatible).

### New features

Plotting functions:

- [`orbi_plot_satellite_peaks()`](https://isoorbi.isoverse.org/dev/reference/orbi_plot_satellite_peaks.md)
  implemented
- [`orbi_plot_isotopocule_coverage()`](https://isoorbi.isoverse.org/dev/reference/orbi_isotopocule_coverage.md)
  implemented
- [`orbi_plot_raw_data()`](https://isoorbi.isoverse.org/dev/reference/orbi_plot_raw_data.md)
  implemented

Information functions:

- [`orbi_get_isotopocule_coverage()`](https://isoorbi.isoverse.org/dev/reference/orbi_isotopocule_coverage.md)

### Enhancements

- [`orbi_simplify_isox()`](https://isoorbi.isoverse.org/dev/reference/orbi_simplify_isox.md)
  now includes `intensity` by default and allows addition of
  user-specified columns
- [`orbi_flag_satellite_peaks()`](https://isoorbi.isoverse.org/dev/reference/orbi_flag_satellite_peaks.md),
  [`orbi_flag_weak_isotopocules()`](https://isoorbi.isoverse.org/dev/reference/orbi_flag_weak_isotopocules.md)
  and
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/dev/reference/orbi_flag_outliers.md)
  now provide additional information when no data is flagged and how to
  visualize what was flagged (if anything)

### Bug Fixes

- [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/dev/reference/orbi_add_blocks_to_plot.md)
  now supports log scale axes

## isoorbi 1.2.0

This is a minor release adding new features.

### Breaking changes

There are no breaking changes in this release (all changes and new
features are backwards compatible).

### New features

- [`orbi_find_isox()`](https://isoorbi.isoverse.org/dev/reference/orbi_find_isox.md)
  added
- `orbi_analyze_noise()` implemented
- [`orbi_plot_shot_noise()`](https://isoorbi.isoverse.org/dev/reference/orbi_plot_shot_noise.md)
  implemented
- [`orbi_flag_satellite_peaks()`](https://isoorbi.isoverse.org/dev/reference/orbi_flag_satellite_peaks.md),
  [`orbi_flag_weak_isotopocules()`](https://isoorbi.isoverse.org/dev/reference/orbi_flag_weak_isotopocules.md)
  and
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/dev/reference/orbi_flag_outliers.md)
  supersede
  [`orbi_filter_satellite_peaks()`](https://isoorbi.isoverse.org/dev/reference/orbi_filter_satellite_peaks.md),
  [`orbi_filter_weak_isotopocules()`](https://isoorbi.isoverse.org/dev/reference/orbi_filter_weak_isotopocules.md)
  and
  [`orbi_filter_scan_intensity()`](https://isoorbi.isoverse.org/dev/reference/orbi_filter_scan_intensity.md)
- [`orbi_export_data_to_excel()`](https://isoorbi.isoverse.org/dev/reference/orbi_export_data_to_excel.md)
  implemented
- [`orbi_define_block_for_flow_injection()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_block_for_flow_injection.md)
  implemented
- [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/dev/reference/orbi_add_blocks_to_plot.md)
  implemented

### Enhancements

- all loading and utility functions provide more details in the info
  message about what has been done, what was affected, and how long it
  took
- all loading and utility functions catch processing errors and report
  the information back to the user
- [`orbi_read_isox()`](https://isoorbi.isoverse.org/dev/reference/orbi_read_isox.md)
  now can read multiple .isox files at once and includes the .isox
  `filepath` in the resulting tibble

### Bug fixes

- data frame groupings introduced in flagging/filtering functions are
  now removed again at the end of the function

## isoorbi 1.1.0

CRAN release: 2023-06-24

This is a minor release adding new features and fixing a few bugs.

### Breaking changes

There are no breaking changes in this release (all changes and new
features are backwards compatible).

### New features

- implemented functionality for dual inlet data acquisitions, see
  [`orbi_define_blocks_for_dual_inlet()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_blocks_for_dual_inlet.md)
  and
  [`orbi_get_blocks_info()`](https://isoorbi.isoverse.org/dev/reference/orbi_get_blocks_info.md)
- implemented package settings, using internal functions
  [`isoorbi::orbi_get_settings`](https://isoorbi.isoverse.org/dev/reference/orbi_get_settings.md),
  [`isoorbi::orbi_set_settings`](https://isoorbi.isoverse.org/dev/reference/orbi_set_settings.md)

### Enhancements

- removed dependency on `stringr`
- implemented native pipe `|>` (R version requirement increased to
  4.1.0) and removed dependency on `magrittr`
- added `.by` parameter for
  [`orbi_summarize_results()`](https://isoorbi.isoverse.org/dev/reference/orbi_summarize_results.md)
  for option to manually adjust grouping

### Bug fixes

- `dplyr` changes to joins with explicit `multiple` argument. `multiple`
  now defaults to “all”, and the options of “error” and “warning” are
  now deprecated in favor of using `relationship`.
