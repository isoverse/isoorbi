# Binning raw data into blocks for dual inlet analyses

This function sorts out (bins) data into indivual blocks of reference,
sample, changeover time, and startup time.

## Usage

``` r
orbi_define_blocks_for_dual_inlet(
  dataset,
  ref_block_time.min,
  change_over_time.min,
  sample_block_time.min = ref_block_time.min,
  startup_time.min = 0,
  ref_block_name = orbi_get_option("di_ref_name"),
  sample_block_name = orbi_get_option("di_sample_name")
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

- ref_block_time.min:

  time where the signal is stable when reference is analyzed

- change_over_time.min:

  time where the signal is unstable after switching from reference to
  sample or back

- sample_block_time.min:

  time where the signal is stable when sample is analyzed

- startup_time.min:

  initial time to stabilize spray

- ref_block_name:

  the name of the reference being measured

- sample_block_name:

  the name of the sample being measured

## Value

A data frame (tibble) with block annotations in the form of the
additional columns described below:

- `data_group` is an integer that numbers each data group (whether
  that's startup, a sample block, a segment, etc.) in each file
  sequentially to uniquely identify groups of data that belong
  together - this columns is NOT static (i.e. functions like
  [`orbi_adjust_block()`](https://isoorbi.isoverse.org/reference/orbi_adjust_block.md)
  and
  [`orbi_segment_blocks()`](https://isoorbi.isoverse.org/reference/orbi_segment_blocks.md)
  will lead to renumbering) and should be used purely for grouping
  purposes in calculations and visualization

- `block` is an integer counting the data blocks in each file (0 is the
  startup block)

- `sample_name` is the name of the material being measured as defined by
  the `ref_block_name` and `sample_block_name` parameters

- `segment` is an integer defines segments within individual blocks -
  this will be `NA` until the optional
  [`orbi_segment_blocks()`](https://isoorbi.isoverse.org/reference/orbi_segment_blocks.md)
  is called

- `data_type` is a text value describing the type of data in each
  `data_group` - for a list of the main categories, call
  `orbi_get_options("data_type")`
