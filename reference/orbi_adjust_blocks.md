# Manually adjust block delimiters

This function can be used to manually adjust where certain blocks start
or end after they have been defined with
[`orbi_define_blocks()`](https://isoorbi.isoverse.org/reference/orbi_define_blocks.md)
or
[`orbi_define_blocks_for_dual_inlet()`](https://isoorbi.isoverse.org/reference/orbi_define_blocks_for_dual_inlet.md)
using either time or scan number. The adjustments can be provided as
vectors in the individual parameters or as a `blocks_table` - whichever
is more convenient. Note that adjusting blocks removes all block
segmentation. Make sure to call
[`orbi_segment_blocks()`](https://isoorbi.isoverse.org/reference/orbi_segment_blocks.md)
**after** adjusting block delimiters.

## Usage

``` r
orbi_adjust_blocks(
  dataset,
  block = NULL,
  in_filename = NULL,
  shift_start_time.min = NULL,
  shift_end_time.min = NULL,
  shift_start_scan.no = NULL,
  shift_end_scan.no = NULL,
  set_start_time.min = NULL,
  set_end_time.min = NULL,
  set_start_scan.no = NULL,
  set_end_scan.no = NULL,
  blocks_table = NULL
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

  the block(s) for which to adjust the start and/or end, a single value
  or a vector for multiple adjustments. Note that blocks are numbered
  within each file.

- in_filename:

  the file(s) in which to adjust the block(s), a single value or a
  vector for multiple adjustments, `NA` (the default if not provided)
  adjusts the block in all files that have it. This must be the
  `filename` of the file(s) as it appears in the `dataset`.

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

- blocks_table:

  alternative to the individual parameters: a data frame with the column
  `block` and any of the columns `in_filename`, `shift_start_time.min`,
  `shift_end_time.min`, `shift_start_scan.no`, `shift_end_scan.no`,
  `set_start_time.min`, `set_end_time.min`, `set_start_scan.no` and
  `set_end_scan.no`, one row per adjustment. Any other columns are
  ignored. If provided, the individual parameters are not used.

## Value

A data frame (tibble) with block limits altered according to the
provided start/end change parameters. Any data that is no longer part of
the original block will be marked with the value of
`orbi_get_option("data_type_unused")`. Any previously applied
segmentation of the adjusted blocks' files will be discarded (`segment`
column set to `NA`) to avoid unintended side effects.

## Details

Each adjustment can change the start of a block (with only one of
`shift_start_time.min`, `shift_start_scan.no`, `set_start_time.min`, or
`set_start_scan.no`) and/or the end of a block (with only one of
`shift_end_time.min`, `shift_end_scan.no`, `set_end_time.min`, or
`set_end_scan.no`). The adjustments are applied in the order they are
provided.

## Examples

``` r
fpath <- system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")
df <- orbi_read_isox(file = fpath) |>
  orbi_simplify_isox() |>
  orbi_define_blocks_for_dual_inlet(
    ref_block_time.min = 0.5,
    change_over_time.min = 0.1
  )
#> ✔ [120ms] orbi_read_isox() loaded 5.18k peaks for 1 compound (NO3-) with 6
#> isotopocules (15N, 17O, 18O, 15N18O, 17O18O, and 18O18O) from
#> testfile_dual_inlet.isox
#> ✔ [4ms] orbi_simplify_isox() kept columns filepath, filename, scan.no,
#> time.min, compound, isotopocule, ions.incremental, tic, and it.ms
#> ✔ [26ms] orbi_define_blocks_for_dual_inlet() identified 6 blocks (3 ref, 3 sam)
#> in data from 1 file

# shift the start of block 1 by 6 seconds in all files
df |> orbi_adjust_blocks(block = 1, shift_start_time.min = 0.1)
#> ✔ [37ms] orbi_adjust_blocks() adjusted 1 block in 1 file
#>  → block 1 in 20220125_01: moved start from scan 1 (300ms) to 29 (6.4s)
#> # A tibble: 5,184 × 14
#>    filepath      filename scan.no time.min compound isotopocule ions.incremental
#>    <chr>         <fct>      <int>    <dbl> <fct>    <fct>                  <dbl>
#>  1 /home/runner… 2022012…       1    0.005 NO3-     15N                   74181.
#>  2 /home/runner… 2022012…       1    0.005 NO3-     17O                   23132.
#>  3 /home/runner… 2022012…       1    0.005 NO3-     18O                  171489.
#>  4 /home/runner… 2022012…       1    0.005 NO3-     15N18O                  746.
#>  5 /home/runner… 2022012…       1    0.005 NO3-     17O18O                  122.
#>  6 /home/runner… 2022012…       1    0.005 NO3-     18O18O                  405.
#>  7 /home/runner… 2022012…       2    0.008 NO3-     15N                   73402.
#>  8 /home/runner… 2022012…       2    0.008 NO3-     17O                   23859.
#>  9 /home/runner… 2022012…       2    0.008 NO3-     18O                  172534.
#> 10 /home/runner… 2022012…       2    0.008 NO3-     15N18O                  759.
#> # ℹ 5,174 more rows
#> # ℹ 7 more variables: tic <dbl>, it.ms <dbl>, data_group <int>, block <int>,
#> #   block_name <chr>, data_type <chr>, segment <int>

# several adjustments at once
df |> orbi_adjust_blocks(
  block = c(1, 2),
  shift_start_time.min = c(0.1, 0.05),
  shift_end_time.min = c(NA, -0.05)
)
#> ✔ [91ms] orbi_adjust_blocks() adjusted 2 blocks in 1 file
#>  → block 1 in 20220125_01: moved start from scan 1 (300ms) to 29 (6.4s)
#>  → block 2 in 20220125_01: moved start from scan 167 (36.1s) to 181 (39.1s)
#>  → block 2 in 20220125_01: moved end from scan 277 (59.8s) to 262 (56.6s)
#> # A tibble: 5,184 × 14
#>    filepath      filename scan.no time.min compound isotopocule ions.incremental
#>    <chr>         <fct>      <int>    <dbl> <fct>    <fct>                  <dbl>
#>  1 /home/runner… 2022012…       1    0.005 NO3-     15N                   74181.
#>  2 /home/runner… 2022012…       1    0.005 NO3-     17O                   23132.
#>  3 /home/runner… 2022012…       1    0.005 NO3-     18O                  171489.
#>  4 /home/runner… 2022012…       1    0.005 NO3-     15N18O                  746.
#>  5 /home/runner… 2022012…       1    0.005 NO3-     17O18O                  122.
#>  6 /home/runner… 2022012…       1    0.005 NO3-     18O18O                  405.
#>  7 /home/runner… 2022012…       2    0.008 NO3-     15N                   73402.
#>  8 /home/runner… 2022012…       2    0.008 NO3-     17O                   23859.
#>  9 /home/runner… 2022012…       2    0.008 NO3-     18O                  172534.
#> 10 /home/runner… 2022012…       2    0.008 NO3-     15N18O                  759.
#> # ℹ 5,174 more rows
#> # ℹ 7 more variables: tic <dbl>, it.ms <dbl>, data_group <int>, block <int>,
#> #   block_name <chr>, data_type <chr>, segment <int>

# the same via a blocks table
df |> orbi_adjust_blocks(
  blocks_table = tibble::tibble(
    block = c(1, 2),
    shift_start_time.min = c(0.1, 0.05),
    shift_end_time.min = c(NA, -0.05)
  )
)
#> ✔ [93ms] orbi_adjust_blocks() adjusted 2 blocks in 1 file
#>  → block 1 in 20220125_01: moved start from scan 1 (300ms) to 29 (6.4s)
#>  → block 2 in 20220125_01: moved start from scan 167 (36.1s) to 181 (39.1s)
#>  → block 2 in 20220125_01: moved end from scan 277 (59.8s) to 262 (56.6s)
#> # A tibble: 5,184 × 14
#>    filepath      filename scan.no time.min compound isotopocule ions.incremental
#>    <chr>         <fct>      <int>    <dbl> <fct>    <fct>                  <dbl>
#>  1 /home/runner… 2022012…       1    0.005 NO3-     15N                   74181.
#>  2 /home/runner… 2022012…       1    0.005 NO3-     17O                   23132.
#>  3 /home/runner… 2022012…       1    0.005 NO3-     18O                  171489.
#>  4 /home/runner… 2022012…       1    0.005 NO3-     15N18O                  746.
#>  5 /home/runner… 2022012…       1    0.005 NO3-     17O18O                  122.
#>  6 /home/runner… 2022012…       1    0.005 NO3-     18O18O                  405.
#>  7 /home/runner… 2022012…       2    0.008 NO3-     15N                   73402.
#>  8 /home/runner… 2022012…       2    0.008 NO3-     17O                   23859.
#>  9 /home/runner… 2022012…       2    0.008 NO3-     18O                  172534.
#> 10 /home/runner… 2022012…       2    0.008 NO3-     15N18O                  759.
#> # ℹ 5,174 more rows
#> # ℹ 7 more variables: tic <dbl>, it.ms <dbl>, data_group <int>, block <int>,
#> #   block_name <chr>, data_type <chr>, segment <int>
```
