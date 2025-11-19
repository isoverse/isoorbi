# Shot noise calculation

This function computes the shot noise calculation.

## Usage

``` r
orbi_analyze_shot_noise(dataset, include_flagged_data = FALSE)
```

## Arguments

- dataset:

  An aggregated dataset or a data frame of peaks (i.e. works directly
  after
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
  as well as with a tibble from [orbi_get_data(peaks =
  everything())](https://isoorbi.isoverse.org/reference/orbi_get_data.md)
  or when reading from an IsoX file)

- include_flagged_data:

  whether to include flagged data in the shot noise calculation (FALSE
  by default)

## Value

The processed data frame with new columns: `n_effective_ions`, `ratio`,
`ratio_rel_se.permil`, `shot_noise.permil`

## Details

Analyze shot noise

will calculate for all combinations of `filename`, `compound`, and
`isotopocule` in the provided `dataset`
