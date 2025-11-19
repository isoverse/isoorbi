# Filter isotopocules

This function helps filter out missing isotopcules, unidentified peaks,
or select for specific isotopocule. It can be called any time after
[`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
or after reading from an isox file. By default (i.e. if run without
setting any parameters), it removes unidentified peaks and missing
isotopcules and keeps all others.

## Usage

``` r
orbi_filter_isotopocules(
  dataset,
  isotopocules = c(),
  keep_missing = FALSE,
  keep_unidentified = FALSE
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

- isotopocules:

  if provided, only these isotopocules will be kept

- keep_missing:

  whether to keep missing isotopocules in the peaks list (i.e. those
  that should be there but are not), default is not to keep them

- keep_unidentified:

  whether to keep unidentified isotopocules in the peaks list (i.e.
  peaks that have not been identified as a specificic isotopocule),
  default is not to keep them

## Value

the `dataset` but filtered for these isotopocules
