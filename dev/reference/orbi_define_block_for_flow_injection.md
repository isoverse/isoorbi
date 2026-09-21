# Define data block for flow injection

**\[deprecated\]**

`orbi_define_block_for_flow_injection()` was renamed
[`orbi_define_blocks()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_blocks.md)
since it is not specific to flow injection and can now define several
blocks at once.

## Usage

``` r
orbi_define_block_for_flow_injection(
  dataset,
  start_time.min = NULL,
  end_time.min = NULL,
  start_scan.no = NULL,
  end_scan.no = NULL,
  block_name = NA_character_,
  sample_name = lifecycle::deprecated()
)
```

## Arguments

- dataset:

  An aggregated dataset or a data frame of peaks (i.e. works directly
  after
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/dev/reference/orbi_identify_isotopocules.md)
  as well as with a tibble from [orbi_get_data(peaks =
  everything())](https://isoorbi.isoverse.org/dev/reference/orbi_get_data.md)
  or when reading from an IsoX file)

- start_time.min:

  start time of the block(s), a single value or a vector for multiple
  blocks

- end_time.min:

  end time of the block(s), a single value or a vector for multiple
  blocks

- start_scan.no:

  start scan of the block(s), a single value or a vector for multiple
  blocks

- end_scan.no:

  end scan of the block(s), a single value or a vector for multiple
  blocks

- block_name:

  name(s) for the block(s), a single value or a vector for multiple
  blocks, `NA` by default (i.e. unnamed)

- sample_name:

  **\[deprecated\]** renamed to `block_name` since the column it sets
  names the block rather than necessarily a sample

## Value

see
[`orbi_define_blocks()`](https://isoorbi.isoverse.org/dev/reference/orbi_define_blocks.md)
