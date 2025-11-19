# Define data block for flow injection

Define a data block by either start and end time or start and end scan
number. If you want to make segments in the blocks (optional), note that
this function - manually defining blocks - removes all block
segmentation. Make sure to call
[`orbi_segment_blocks()`](https://isoorbi.isoverse.org/reference/orbi_segment_blocks.md)
**only after** finishing block definitions.

## Usage

``` r
orbi_define_block_for_flow_injection(
  dataset,
  start_time.min = NULL,
  end_time.min = NULL,
  start_scan.no = NULL,
  end_scan.no = NULL,
  sample_name = NULL
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

- start_time.min:

  set the start time of the block

- end_time.min:

  set the end time of the block

- start_scan.no:

  set the start scan of the block

- end_scan.no:

  set the end scan of the block

- sample_name:

  if provided, will be used as the `sample_name` for the block

## Value

A data frame (tibble) with block definition added. Any data that is not
part of a block will be marked with the value of
`orbi_get_option("data_type_unused")`. Any previously applied
segmentation will be discarded (`segment` column set to `NA`) to avoid
unintended side effects.
