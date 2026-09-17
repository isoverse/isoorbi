# isoraw 0.3.0

This isoraw release is used by isoorbi version 1.6.0+.

## Breaking changes

 * `peaks.parquet` no longer contains the `is_ref` and `is_lock_peak` columns. The raw Thermo `PeakOptions` bitmask is instead reported in the `flags` column and both of these (and every other flag) can be derived from it downstream.
 * **all** peaks are now read by default. Previously the reader silently kept only the peaks that carried no flags, the reference flag or the lock mass flag, and discarded the rest. Use the new `--skipProblematicPeaks` option to get the previous behavior.

## New features

 * new `--skipProblematicPeaks` option that keeps only the unproblematic peaks, i.e. those whose flags are purely none, reference and/or lock mass. Any other flag (saturated, fragmented, merged, exception, modified) marks a peak as problematic, also in combination with the reference/lock mass flag. See the README for details and for the size impact of keeping the problematic peaks.
 * added a smoke test suite (`test.sh`, `rake test`, test files in `tests/`) that is run on Linux, Windows and macOS by the `isoraw test` GitHub action.

# isoraw 0.2.2

 * the `file_info` dataset now includes the `InAquisition` field.
 * reading a raw file that is still being acquired now emits a warning instead of aborting. Support for this is not fully tested.

# isoraw 0.2.1

 * fixed an issue with partially written parquet files by deleting an existing file before writing it.

Not released separately, included in the 0.2.2 release.

# isoraw 0.2.0

First version of the isoraw reader released with isoorbi. Reads Thermo `.raw` files via Thermo's [RawFileReader](https://github.com/thermofisherlsms/RawFileReader) and writes `file_info`, `scans`, `peaks` and (optionally) `spectra` to `.parquet` files in a `<file>.raw.cache` folder, with `--file`, `--skip` and `--spectra` options plus `--version`/`--help`.
