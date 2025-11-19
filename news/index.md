# Changelog

## isoorbi 1.5.3

This is a minor update to support the latest version of testthat and
address a few small bugs.

### Bug fixes & improvements

- updated snapshots for continuous integration tests to work with
  testthat 3.3.0
- changed
  [`orbi_export_data_to_excel()`](https://isoorbi.isoverse.org/reference/orbi_export_data_to_excel.md)
  to only export `file_info` and `summary` by default to avoid
  uintentionally enormous excel files when exporting large datasets
  ([\#106](https://github.com/isoverse/isoorbi/issues/106))
- fixed issue with
  [`orbi_define_basepeak()`](https://isoorbi.isoverse.org/reference/orbi_define_basepeak.md)
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
  [`orbi_get_example_files()`](https://isoorbi.isoverse.org/reference/orbi_get_example_files.md)
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
  [`orbi_read_raw()`](https://isoorbi.isoverse.org/reference/orbi_read_raw.md)
- implemented versatile isotopocule mapping via
  `orbi_identifiy_isotopocule()` from csv, tsv, and excel inputs
- implemented
  [`orbi_calculate_ions()`](https://isoorbi.isoverse.org/reference/orbi_calculate_ions.md)
  to calculate ions from peak intensities and noise with flexible `CN`
  and `RN` parameters
- implemented
  [`orbi_plot_spectra()`](https://isoorbi.isoverse.org/reference/orbi_plot_spectra.md)
  to plot

### Enhancements

- upgraded all data processing and plotting functions to be compatible
  with both isox and raw file datasets
- improved default print outputs for S3 classes used in the package
- improved package structure documentation
- expanded
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/reference/orbi_flag_outliers.md)
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
  ([`orbi_options()`](https://isoorbi.isoverse.org/reference/orbi_options.md)
  replaces
  [`orbi_set_settings()`](https://isoorbi.isoverse.org/reference/orbi_set_settings.md)
  and
  [`orbi_get_options()`](https://isoorbi.isoverse.org/reference/orbi_options.md)
  /
  [`orbi_get_option()`](https://isoorbi.isoverse.org/reference/orbi_options.md)
  replaces
  [`orbi_get_settings()`](https://isoorbi.isoverse.org/reference/orbi_get_settings.md))
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

- [`orbi_plot_satellite_peaks()`](https://isoorbi.isoverse.org/reference/orbi_plot_satellite_peaks.md)
  implemented
- [`orbi_plot_isotopocule_coverage()`](https://isoorbi.isoverse.org/reference/orbi_isotopocule_coverage.md)
  implemented
- [`orbi_plot_raw_data()`](https://isoorbi.isoverse.org/reference/orbi_plot_raw_data.md)
  implemented

Information functions:

- [`orbi_get_isotopocule_coverage()`](https://isoorbi.isoverse.org/reference/orbi_isotopocule_coverage.md)

### Enhancements

- [`orbi_simplify_isox()`](https://isoorbi.isoverse.org/reference/orbi_simplify_isox.md)
  now includes `intensity` by default and allows addition of
  user-specified columns
- [`orbi_flag_satellite_peaks()`](https://isoorbi.isoverse.org/reference/orbi_flag_satellite_peaks.md),
  [`orbi_flag_weak_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_flag_weak_isotopocules.md)
  and
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/reference/orbi_flag_outliers.md)
  now provide additional information when no data is flagged and how to
  visualize what was flagged (if anything)

### Bug Fixes

- [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md)
  now supports log scale axes

## isoorbi 1.2.0

This is a minor release adding new features.

### Breaking changes

There are no breaking changes in this release (all changes and new
features are backwards compatible).

### New features

- [`orbi_find_isox()`](https://isoorbi.isoverse.org/reference/orbi_find_isox.md)
  added
- `orbi_analyze_noise()` implemented
- [`orbi_plot_shot_noise()`](https://isoorbi.isoverse.org/reference/orbi_plot_shot_noise.md)
  implemented
- [`orbi_flag_satellite_peaks()`](https://isoorbi.isoverse.org/reference/orbi_flag_satellite_peaks.md),
  [`orbi_flag_weak_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_flag_weak_isotopocules.md)
  and
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/reference/orbi_flag_outliers.md)
  supersede
  [`orbi_filter_satellite_peaks()`](https://isoorbi.isoverse.org/reference/orbi_filter_satellite_peaks.md),
  [`orbi_filter_weak_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_weak_isotopocules.md)
  and
  [`orbi_filter_scan_intensity()`](https://isoorbi.isoverse.org/reference/orbi_filter_scan_intensity.md)
- [`orbi_export_data_to_excel()`](https://isoorbi.isoverse.org/reference/orbi_export_data_to_excel.md)
  implemented
- [`orbi_define_block_for_flow_injection()`](https://isoorbi.isoverse.org/reference/orbi_define_block_for_flow_injection.md)
  implemented
- [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md)
  implemented

### Enhancements

- all loading and utility functions provide more details in the info
  message about what has been done, what was affected, and how long it
  took
- all loading and utility functions catch processing errors and report
  the information back to the user
- [`orbi_read_isox()`](https://isoorbi.isoverse.org/reference/orbi_read_isox.md)
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
  [`orbi_define_blocks_for_dual_inlet()`](https://isoorbi.isoverse.org/reference/orbi_define_blocks_for_dual_inlet.md)
  and
  [`orbi_get_blocks_info()`](https://isoorbi.isoverse.org/reference/orbi_get_blocks_info.md)
- implemented package settings, using internal functions
  [`isoorbi::orbi_get_settings`](https://isoorbi.isoverse.org/reference/orbi_get_settings.md),
  [`isoorbi::orbi_set_settings`](https://isoorbi.isoverse.org/reference/orbi_set_settings.md)

### Enhancements

- removed dependency on `stringr`
- implemented native pipe `|>` (R version requirement increased to
  4.1.0) and removed dependency on `magrittr`
- added `.by` parameter for
  [`orbi_summarize_results()`](https://isoorbi.isoverse.org/reference/orbi_summarize_results.md)
  for option to manually adjust grouping

### Bug fixes

- `dplyr` changes to joins with explicit `multiple` argument. `multiple`
  now defaults to “all”, and the options of “error” and “warning” are
  now deprecated in favor of using `relationship`.
