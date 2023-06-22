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
