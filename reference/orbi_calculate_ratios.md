# Calculate direct isotopocule ratios

This function calculates isotopocule/base peak ratios for all
isotopocules. It does not summarize or average the ratios in any way.
For a summarizing version of this function, see
[`orbi_summarize_results()`](https://isoorbi.isoverse.org/reference/orbi_summarize_results.md).

## Usage

``` r
orbi_calculate_ratios(dataset)
```

## Arguments

- dataset:

  A data frame output after running
  [`orbi_define_basepeak()`](https://isoorbi.isoverse.org/reference/orbi_define_basepeak.md)

## Value

Returns a mutated dataset with `ratio` column added.
