# Define the denominator for ratio calculation

This function sets one isotopocule in the data frame as the base peak
(ratio denominator) and calculates the instantaneous isotope ratios
against it. Note that any unidentified or missing peaks in the dataset
are filtered out prior to ratio calculations (i.e. what
[`orbi_filter_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.md)
does by default) to avoid ambiguity in the dataset. To do this
explicitly (with information displayed about how many unidentified peaks
were filtered out), call
[`orbi_filter_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.md)
manually before `orbi_define_basepeak()`.

## Usage

``` r
orbi_define_basepeak(dataset, basepeak_def)
```

## Arguments

- dataset:

  An aggregated dataset or a data frame of peaks (i.e. works directly
  after
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
  as well as with a tibble from [orbi_get_data(peaks =
  everything())](https://isoorbi.isoverse.org/reference/orbi_get_data.md)
  or when reading from an IsoX file)

- basepeak_def:

  The isotopocule that gets defined as base peak, i.e. the denominator
  to calculate ratios

## Value

same object as provided in `dataset` without the rows of the basepeak
isotopocule and instead three new columns called `basepeak`,
`basepeak_ions`, and `ratio` holding the basepeak information and the
isotope ratios vs. the base peak

## Examples

``` r
fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
df <- orbi_read_isox(file = fpath) |>
  orbi_simplify_isox() |>
  orbi_define_basepeak(basepeak_def = "M0")
#> ✔ [22ms] orbi_read_isox() loaded 6449 peaks for 1 compound (HSO4-) with 5
#> isotopocules (M0, 33S, 17O, 34S, and 18O) from testfile_flow.isox
#> ✔ [6ms] orbi_simplify_isox() kept columns filepath, filename, scan.no,
#> time.min, compound, isotopocule, ions.incremental, tic, and it.ms
#> ✔ [149ms] orbi_define_basepeak() set M0 as the ratio denominator and calculated
#> 5.16k ratio values for 4 isotopocules (33S, 17O, 34S, and 18O)
```
