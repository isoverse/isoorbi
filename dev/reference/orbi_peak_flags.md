# Peak flags

The raw file reader reports the `PeakOptions` bitmask the instrument
assigned to each peak. The built-in aggregators (see
[`orbi_aggregate_raw()`](https://isoorbi.isoverse.org/dev/reference/orbi_aggregate_raw.md))
decode it with `orbi_peak_flags_to_text()` and provide it as the
readable `centroiderFlags` factor column of the `peaks` dataset. The
name sets these apart from the peaks that `isoorbi` itself flags later
on (satellite peaks, weak isotopocules and outliers).

## Usage

``` r
orbi_peak_flags_include(flags, flag)

orbi_peak_flags_to_text(flags)
```

## Arguments

- flags:

  the peak flags, i.e. the decoded `centroiderFlags` column of the
  `peaks` dataset or the raw numeric `PeakOptions` bitmask reported by
  the reader. `orbi_peak_flags_include()` accepts either and gives the
  same result for both, `orbi_peak_flags_to_text()` decodes the bitmask.

- flag:

  one or more flag names, see the list of available flags in the details
  below (`"none"` is not a flag a peak can include, see above)

## Value

a vector of the same length as `flags`: logical for
`orbi_peak_flags_include()`, character for `orbi_peak_flags_to_text()`

## Details

Filtering for an *exact* set of flags does not need any function -
compare that column directly, e.g.
`dplyr::filter(peaks, centroiderFlags == "reference")` for the peaks
that are exclusively a reference peak, or
`dplyr::filter(peaks, centroiderFlags == "none")` for those without any
flags. `orbi_peak_flags_include()` covers the case a comparison cannot:
finding peaks that carry a flag *irrespective* of which others they
carry, which would otherwise require a comparatively expensive regular
expression search. It accepts both the decoded text and the raw numeric
bitmask and returns the same result for either.

The available flags are `"exception"` (part of the reference but not
used by calibration), `"fragmented"` (peak split by the centroider),
`"lock peak"` (high resolution SIM lock mass), `"merged"` (peaks
combined by the centroider), `"modified"` (mathematically modified
packet), `"reference"` (hi-res internal reference compound) and
`"saturated"` (signal over the ADC limit), plus `"none"` for peaks that
carry no flags at all.

Note that only `"reference"` and `"lock peak"` describe what a peak
*is* - every other flag reports a problem with the centroiding, so a
reference or lock mass peak that carries any additional flag is
problematic as well.

## Functions

- `orbi_peak_flags_include()`: whether the peak's flags include all of
  the provided `flag`(s), i.e. additional flags may be present as well
  but every provided flag has to be. Accepts either the decoded text or
  the raw bitmask and returns the same result for both. To test for
  *any* of several flags, combine the individual calls with `|`. Note
  that `"none"` is not a flag a peak can include - compare the column
  instead (`dplyr::filter(peaks, centroiderFlags == "none")`) for peaks
  without any flags.

- `orbi_peak_flags_to_text()`: convert the flags to an alphabetically
  sorted, comma separated text representation, e.g. `10` becomes
  `"exception + fragmented"`. Peaks without any flags become `"none"`.

## Examples

``` r
# the `centroiderFlags` column of an aggregated dataset holds the decoded flags
flags <- c("none", "exception", "reference", "fragmented + reference", "lock peak")

# any peak that carries the reference flag, whether or not it carries others
orbi_peak_flags_include(flags, "reference")
#> [1] FALSE FALSE  TRUE  TRUE FALSE

# several flags have to ALL be present
orbi_peak_flags_include(flags, c("reference", "fragmented"))
#> [1] FALSE FALSE FALSE  TRUE FALSE

# for any of several flags, combine the individual calls
orbi_peak_flags_include(flags, "reference") |
  orbi_peak_flags_include(flags, "lock peak")
#> [1] FALSE FALSE  TRUE  TRUE  TRUE

# an exact set of flags does not need a function, compare the column directly
flags == "reference"
#> [1] FALSE FALSE  TRUE FALSE FALSE
flags == "none"
#> [1]  TRUE FALSE FALSE FALSE FALSE

# problematic peaks, i.e. anything that is not purely unflagged/reference/lock mass
!flags %in% c("none", "reference", "lock peak", "lock peak + reference")
#> [1] FALSE  TRUE FALSE  TRUE FALSE

# the raw bitmask reported by the reader works just as well
bitmask <- c(0L, 8L, 16L, 18L, 64L)
orbi_peak_flags_to_text(bitmask)
#> [1] "none"                   "exception"              "reference"             
#> [4] "fragmented + reference" "lock peak"             
orbi_peak_flags_include(bitmask, "reference")
#> [1] FALSE FALSE  TRUE  TRUE FALSE
```
