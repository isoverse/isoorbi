# Basic generic data files filter

This is a basic filter function for file names, compounds and time
ranges. For filtering isotopocules, this function calls
[`orbi_filter_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.md)
internally (as of isoorbi version 1.5.0
[`orbi_filter_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.md)
can also be used directly instead of via this function). Default value
for all parameters is NULL, i.e. no filter is applied.

## Usage

``` r
orbi_filter_files(
  dataset,
  filenames = NULL,
  compounds = NULL,
  isotopocules = NULL,
  time_min = NULL,
  time_max = NULL
)
```

## Arguments

- dataset:

  An aggregated dataset or a data frame of peaks (i.e. works directly
  after
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
  as well as with a tibble from [orbi_get_data(peaks =
  everything())](https://isoorbi.isoverse.org/reference/orbi_get_data.md)
  or when reading from an IsoX file)

- filenames:

  Vector of file names to keep, keeps all if set to `NULL` (the default)

- compounds:

  Vector of compounds to keep, keeps all if set to `NULL` (the default)

- isotopocules:

  Vector of isotopocules to keep, keeps all if set to `NULL` (the
  default)

- time_min:

  Minimum retention time in minutes (`time.min`), no minimum if set to
  `NULL` (the default)

- time_max:

  Maximum retention time in minutes (`time.min`), no maximum if set to
  `NULL` (the default)

## Value

Filtered tibble

## Examples

``` r
fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
df <-
  orbi_read_isox(file = fpath) |>
  orbi_simplify_isox() |>
  orbi_filter_files(
    filenames = c("s3744"),
    compounds = "HSO4-",
    isotopocules = c("M0", "34S", "18O")
  )
#> ✔ [21ms] orbi_read_isox() loaded 6449 peaks for 1 compound (HSO4-) with 5
#> isotopocules (M0, 33S, 17O, 34S, and 18O) from testfile_flow.isox
#> ✔ [5ms] orbi_simplify_isox() kept columns filepath, filename, scan.no,
#> time.min, compound, isotopocule, ions.incremental, tic, and it.ms
#> ✔ [6ms] orbi_filter_isotopocules() removed 860 / 2.15k peaks (40%) because they
#> were not the selected isotopocule M0, 34S, and 18O (860).
#> ✔ [80ms] orbi_filter_files() filtered the dataset by filenames (s3744),
#> compounds (HSO4-), and isotopocules (M0, 34S, 18O) and removed a total of
#> 5.16k/6.45k peaks (80%)
```
