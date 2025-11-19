# Calculate ions from intensities

This functions calculates ions (`ions.incremental`) from intensities
based on the equation \$\$N\_{ions} = S/N \cdot{} C_N/z \cdot{}
\sqrt{R_N/R} \cdot \sqrt{N\_{MS}}\$\$ where S is the reported signal
(`intensity`) of the isotopocule, N is the noise associated with the
signal (`peakNoise`), measured at the resolution setting R
(`resolution`), the noise factor \\C_N\\ (`CN`) is the number of charges
corresponding to the Orbitrap noise band at some reference resolution
\\R_N\\ (`RN`), \\N\_{MS}\\ is the number of microscans, and z is the
charge per ion (`charge`) of the isotopocule. See Makarov and Denisov
(2009) and Eiler et al. (2017) for details about this equation. The
default values for `CN` and `RN` are from the Orbitrap Exploris Isotope
Solutions Getting Started Guide (BRE0032999, Revision A, October 2022).
Note that the exact values of these factors are only critical if the
number of ions are interpreted outside of ratio calculations (in ratio
calculations, these factors cancel).

## Usage

``` r
orbi_calculate_ions(dataset, CN = 3, RN = 240000)
```

## Arguments

- dataset:

  An aggregated dataset or a data frame of peaks (i.e. works directly
  after
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
  as well as with a tibble from [orbi_get_data(peaks =
  everything())](https://isoorbi.isoverse.org/reference/orbi_get_data.md)
  or when reading from an IsoX file)

- CN:

  noise factor

- RN:

  reference resolution of the noise factor

## Value

same object as provided in `dataset` with new column `ions.incremental`

## Details

If using a dataset read from isox files you might have to add a `charge`
column if it does not yet exist that indicates the charge of the
isotopocule. If using data from raw files,
[`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
will automatically call this function with the default `CN` and `RN` so
you don't need to call it explicitly unless you want to change these
parameters.
