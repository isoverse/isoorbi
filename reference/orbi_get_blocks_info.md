# Summarize blocks info

This function provides an overview table `blocks_info` which shows
information on blocks in the dataset (block number, sample name, data
type, scan number and start time where a block starts, and scan number
and end time where a block ends).

## Usage

``` r
orbi_get_blocks_info(
  dataset,
  .by = c("uidx", "filename", "injection", "data_group", "block", "sample_name",
    "data_type", "segment")
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

- .by:

  grouping columns for block info (akin to dplyr's `.by` parameter e.g.
  in
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)).
  If not set by the user, all columns in the parameter's default values
  are used, if present in the dataset.

## Value

a block summary or if no blocks defined yet, an empty tibble (with
warning)
