# Manually adjust block delimiters

This function can be used to manually adjust where certain `block`
starts or ends after it's been defined with
[`orbi_define_block_for_flow_injection()`](https://isoorbi.isoverse.org/reference/orbi_define_block_for_flow_injection.md)
or
[`orbi_define_blocks_for_dual_inlet()`](https://isoorbi.isoverse.org/reference/orbi_define_blocks_for_dual_inlet.md)
using either time or scan number. Note that adjusting blocks removes all
block segmentation. Make sure to call
[`orbi_segment_blocks()`](https://isoorbi.isoverse.org/reference/orbi_segment_blocks.md)
**after** adjusting block delimiters.

## Usage

``` r
orbi_adjust_block(
  dataset,
  block,
  filename = NULL,
  shift_start_time.min = NULL,
  shift_end_time.min = NULL,
  shift_start_scan.no = NULL,
  shift_end_scan.no = NULL,
  set_start_time.min = NULL,
  set_end_time.min = NULL,
  set_start_scan.no = NULL,
  set_end_scan.no = NULL
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

- block:

  the block for which to adjust the start and/or end

- filename:

  needs to be specified only if the `dataset` has more than one
  `filename`

- shift_start_time.min:

  if provided, the start time of the block will be shifted by this many
  minutes (use negative numbers to shift back)

- shift_end_time.min:

  if provided, the end time of the block will be shifted by this many
  minutes (use negative numbers to shift back)

- shift_start_scan.no:

  if provided, the start of the block will be shifted by this many scans
  (use negative numbers to shift back)

- shift_end_scan.no:

  if provided, the end of the block will be shifted by this many scans
  (use negative numbers to shift back)

- set_start_time.min:

  if provided, sets the start time of the block as close as possible to
  this time

- set_end_time.min:

  if provided, sets the end time of the block as close as possible to
  this time

- set_start_scan.no:

  if provided, sets the start of the block to this scan number (scan
  must exist in the `dataset`)

- set_end_scan.no:

  if provided, sets the end of the block to this scan number (scan must
  exist in the `dataset`)

## Value

A data frame (tibble) with block limits altered according to the
provided start/end change parameters. Any data that is no longer part of
the original block will be marked with the value of
`orbi_get_option("data_type_unused")`. Any previously applied
segmentation will be discarded (`segment` column set to `NA`) to avoid
unintended side effects.
