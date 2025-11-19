# Read IsoX file

Read an IsoX dataput file (`.isox`) into a tibble data frame.

## Usage

``` r
orbi_read_isox(file)
```

## Arguments

- file:

  Path to the `.isox` file(s), single value or vector of paths

## Value

A tibble containing at minimum the columns `filename`, `scan.no`,
`time.min`, `compound`, `isotopocule`, `ions.incremental`, `tic`,
`it.ms`

## Details

Additional information on the columns:

- `filename`: name of the original Thermo `.raw` file processed by IsoX

- `scan.no`: scan number

- `time.min`: acquisition or retention time in minutes

- `compound`: name of the compound (e.g., NO3-)

- `isotopocule`: name of the isotopocule (e.g., 15N); called
  `isotopolog` in `.isox`

- `ions.incremental`: estimated number of ions, in increments since it
  is a calculated number

- `tic`: total ion current (TIC) of the scan

- `it.ms`: scan injection time (IT) in millisecond (ms)

## Examples

``` r
fpath <- system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")
df <- orbi_read_isox(file = fpath)
#> ✔ [11ms] orbi_read_isox() loaded 5184 peaks for 1 compound (NO3-) with 6
#> isotopocules (15N, 17O, 18O, 15N18O, 17O18O, and 18O18O) from
#> testfile_dual_inlet.isox
```
