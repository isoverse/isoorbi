# Identify isotopocules

Map the mass spectral peaks to specific isotopocules based on their
mass.

## Usage

``` r
orbi_identify_isotopocules(
  aggregated_data,
  isotopocules,
  default_tolerance = 1,
  default_charge = 1
)
```

## Arguments

- aggregated_data:

  either data aggregated from
  [`orbi_aggregate_raw()`](https://isoorbi.isoverse.org/reference/orbi_aggregate_raw.md)
  or a straight-up tibble data frame of the peaks (e.g. retrieved via
  `orbi_get_data(peaks = everything())`).

- isotopocules:

  list of isotopocules to map, can be a data frame/tibble, a named
  vector such as `c("M0" = 61.9878, "15N" = 62.9850)`, or the name of a
  file to read from (.csv/.tsv/.xlsx are all supported). If provided as
  a tibble/file, the required columns are `isotopocule/isotopolog` and
  `mz/mass` (these alternative names for the columns, including
  uppercase versions, are recognized automatically). In addition,
  `tolerance/tolerance [mmu]/tolerance [mDa]`, `charge/z`,
  `#compound/compound`, and `fragment` are recognized, as well as any
  other (arbitrarily named) columns with additional information.
  Character columns in the `isotopocules` table (including `isotopocule`
  and `compound`) are turned into factors with levels that preserve the
  order of isotopocules. That means that to change the order of
  isotopocules in downstream plotting functions, make sure to list them
  in the order you'd like them presented in. Note that if
  `tolerance/tolerance [mmu]/tolerance [mDa]` or `charge/z` are not
  provided, the values in the parameters `default_tolerance` and
  `default_charge` are used, respectively.

- default_tolerance:

  tolerance (in mmu) to be used for isotopocule identification if a
  `tolerance/tolerance [mmu]/tolerance [mDa]` column is not included in
  `isotopocules`

- default_charge:

  charge to be used for any unidentified peak, and if a `charge/z`
  column is not included in `isotopocules`

## Value

same object as provided in `aggregated_data` with added columns
`compound` (if provided), `itc_uidx` (introduced unique isotopocule
index), `isotopocule`, `mzExact`, `charge`, and `ions.incremental` (via
[`orbi_calculate_ions()`](https://isoorbi.isoverse.org/reference/orbi_calculate_ions.md)),
as as well as any other additional information columns provided in
`isotopocules`. Note that if the default `CN` and `RN` values of
[`orbi_calculate_ions()`](https://isoorbi.isoverse.org/reference/orbi_calculate_ions.md)
are not the ones that should be used, simply run
[`orbi_calculate_ions()`](https://isoorbi.isoverse.org/reference/orbi_calculate_ions.md)
explicitly afterwards. Also note that the information about columns that
were NOT aggregated in previous steps is purposefully not preserved at
this step.
