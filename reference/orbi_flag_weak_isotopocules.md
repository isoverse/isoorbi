# Flag weak isotopocules

This function flags isotopocules that are not detected in a minimum of
`min_percent` of scans that then can be easily visualized with
[`orbi_plot_isotopocule_coverage()`](https://isoorbi.isoverse.org/reference/orbi_isotopocule_coverage.md).
It evaluates weak isotopocules within each "uidx", "filename", "block",
"segment" and "injection" (for those of the columns that exist), in
addition to any groupings already defined before calling this function
using dplyr's `group_by()`. It restores the original groupings in the
returned data.

## Usage

``` r
orbi_flag_weak_isotopocules(dataset, min_percent = 100)
```

## Arguments

- dataset:

  A simplified IsoX data frame to be processed

- min_percent:

  A number between 0 and 100 (inclusive). Isotopocule must be observed
  in at least this percentage of scans (please note: the percentage is
  defined relative to the most commonly observed isotopocule of each
  compound). The default is 100, the most stringent condition to ensure
  reliable isotpocule coverage and ratio calculations across data
  blocks. If you lower the default, be mindful of potential
  misinterprations from using isotopotcules that are very close to their
  detection limit within a datablock. For continuous flow operations it
  may be necessary to make data blocks smaller using
  [`orbi_define_block_for_flow_injection()`](https://isoorbi.isoverse.org/reference/orbi_define_block_for_flow_injection.md)
  and
  [`orbi_adjust_block()`](https://isoorbi.isoverse.org/reference/orbi_adjust_block.md).

## Value

same object as provided in `dataset` with new column
`is_weak_isotopocule` that flags weak isotopocules.

## Examples

``` r
fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
df <- orbi_read_isox(file = fpath) |>
      orbi_simplify_isox() |>
      orbi_flag_weak_isotopocules(min_percent = 100)
#> ✔ [21ms] orbi_read_isox() loaded 6449 peaks for 1 compound (HSO4-) with 5
#> isotopocules (M0, 33S, 17O, 34S, and 18O) from testfile_flow.isox
#> ✔ [6ms] orbi_simplify_isox() kept columns filepath, filename, scan.no,
#> time.min, compound, isotopocule, ions.incremental, tic, and it.ms
#> ✔ [35ms] orbi_flag_weak_isotopocules() flagged 1 of 15 isotopocules as weak
#> because they were NOT present in at least 100% of scans in each of the 15 data
#> groups (based on filename, compound, and isotopocule) → use
#> orbi_plot_isotopocule_coverage() to visualize them
```
