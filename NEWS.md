# isoorbi 1.1.0

This is a major release adding substantial new features and fixing a few bugs.

## Breaking changes

There are no breaking changes in this release (all changes and new features are backwards compatible).

## New features

* implemented block annotation functionality (FIXME: explain more)
* implemented package settings (FIXME: explain more)

## Enhancements

* removed dependency on `stringr`
* implemented native pipe `|>` (R version requirement increased to 4.1.0) and removed dependency on `magrittr`
* added `.by` parameter for `orbi_summarize_results()` for option to manually adjust grouping

## Bug fixes

* `dplyr` changes to joins with explicit `multiple` argument are now implemented (#10)

# isorbi 1.0.0

First public release.
