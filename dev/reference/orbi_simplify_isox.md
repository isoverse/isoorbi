# Simplify IsoX data

Keep only columns that are directly relevant for isotopocule ratio
analysis. This function is optional and does not affect any downstream
function calls.

## Usage

``` r
orbi_simplify_isox(dataset, add = c())
```

## Arguments

- dataset:

  IsoX data that is to be simplified

- add:

  additional columns to keep

## Value

A tibble containing only the 9 columns: `filepath`, `filename`,
`scan.no`, `time.min`, `compound`, `isotopocule`, `ions.incremental`,
`tic`, `it.ms`, plus any additional columns defined in the `add`
argument

## Examples

``` r
fpath <- system.file("extdata", "testfile_flow.isox", package="isoorbi")
df <- orbi_read_isox(file = fpath) |> orbi_simplify_isox()
#> ✔ [20ms] orbi_read_isox() loaded 6449 peaks for 1 compound (HSO4-) with 5
#> isotopocules (M0, 33S, 17O, 34S, and 18O) from testfile_flow.isox
#> ✔ [5ms] orbi_simplify_isox() kept columns filepath, filename, scan.no,
#> time.min, compound, isotopocule, ions.incremental, tic, and it.ms
```
