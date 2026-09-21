# Define data blocks

Define one or more data blocks by either start and end time or start and
end scan number. The blocks can be provided as vectors in the individual
parameters or as a `blocks_table` - whichever is more convenient. If you
want to make segments in the blocks (optional), note that this
function - manually defining blocks - removes all block segmentation.
Make sure to call
[`orbi_segment_blocks()`](https://isoorbi.isoverse.org/dev/reference/orbi_segment_blocks.md)
**only after** finishing block definitions.

## Usage

``` r
orbi_define_blocks(
  dataset,
  start_time.min = NULL,
  end_time.min = NULL,
  start_scan.no = NULL,
  end_scan.no = NULL,
  block_name = NA_character_,
  blocks_table = NULL
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

- blocks_table:

  alternative to the individual parameters: a data frame with any of the
  columns `start_time.min`, `end_time.min`, `start_scan.no`,
  `end_scan.no` and `block_name`, one row per block. Any other columns
  are ignored. If provided, the individual parameters are not used.

## Value

A data frame (tibble) with the block definitions added. Any data that is
not part of a block will be marked with the value of
`orbi_get_option("data_type_unused")`. Any previously applied
segmentation will be discarded (`segment` column set to `NA`) to avoid
unintended side effects.

## Details

Each block has to be defined either by time (`start_time.min` **and**
`end_time.min`) or by scan number (`start_scan.no` **and**
`end_scan.no`) but not by both. Different blocks in the same call can
use different definitions.

Blocks are matched against the scans of each file separately. A block
whose range reaches beyond what a file recorded is trimmed to the scans
that exist in that file, and a block that does not overlap a file at all
is not added there - this is reported with a warning naming the affected
files and the range each of them covers. The summary message lists the
scans and times each block actually ended up covering, which is what to
check if a block was trimmed.

## Examples

``` r
fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
df <- orbi_read_isox(file = fpath) |> orbi_simplify_isox()
#> ✔ [19ms] orbi_read_isox() loaded 6449 peaks for 1 compound (HSO4-) with 5
#> isotopocules (M0, 33S, 17O, 34S, and 18O) from testfile_flow.isox
#> ✔ [5ms] orbi_simplify_isox() kept columns filepath, filename, scan.no,
#> time.min, compound, isotopocule, ions.incremental, tic, and it.ms

# a single block
df |> orbi_define_blocks(start_time.min = 0.2, end_time.min = 0.8)
#> ✔ [48ms] orbi_define_blocks() added 1 block to 3 files
#> • block: covers scans 87 to 344 (0.202 to 0.799 min) in 3 files
#> # A tibble: 6,449 × 14
#>    filepath      filename scan.no time.min compound isotopocule ions.incremental
#>    <chr>         <fct>      <int>    <dbl> <fct>    <fct>                  <dbl>
#>  1 /home/runner… s3744          1    0.002 HSO4-    M0                   37803. 
#>  2 /home/runner… s3744          1    0.002 HSO4-    33S                    326. 
#>  3 /home/runner… s3744          1    0.002 HSO4-    17O                     61.9
#>  4 /home/runner… s3744          1    0.002 HSO4-    34S                   2215. 
#>  5 /home/runner… s3744          1    0.002 HSO4-    18O                    436. 
#>  6 /home/runner… s3744          2    0.004 HSO4-    M0                   42346. 
#>  7 /home/runner… s3744          2    0.004 HSO4-    33S                    339. 
#>  8 /home/runner… s3744          2    0.004 HSO4-    17O                     66.6
#>  9 /home/runner… s3744          2    0.004 HSO4-    34S                   2315. 
#> 10 /home/runner… s3744          2    0.004 HSO4-    18O                    438. 
#> # ℹ 6,439 more rows
#> # ℹ 7 more variables: tic <dbl>, it.ms <dbl>, data_group <int>, block <int>,
#> #   block_name <chr>, data_type <chr>, segment <int>

# several blocks at once
df |> orbi_define_blocks(
  start_time.min = c(0.1, 0.5),
  end_time.min = c(0.4, 0.8),
  block_name = c("first", "second")
)
#> ✔ [85ms] orbi_define_blocks() added 2 blocks to 3 files
#> • block first: covers scans 43 to 172 (0.1 to 0.399 min) in 3 files
#> • block second: covers scans 216 to 344 (0.502 to 0.799 min) in 3 files
#> # A tibble: 6,449 × 14
#>    filepath      filename scan.no time.min compound isotopocule ions.incremental
#>    <chr>         <fct>      <int>    <dbl> <fct>    <fct>                  <dbl>
#>  1 /home/runner… s3744          1    0.002 HSO4-    M0                   37803. 
#>  2 /home/runner… s3744          1    0.002 HSO4-    33S                    326. 
#>  3 /home/runner… s3744          1    0.002 HSO4-    17O                     61.9
#>  4 /home/runner… s3744          1    0.002 HSO4-    34S                   2215. 
#>  5 /home/runner… s3744          1    0.002 HSO4-    18O                    436. 
#>  6 /home/runner… s3744          2    0.004 HSO4-    M0                   42346. 
#>  7 /home/runner… s3744          2    0.004 HSO4-    33S                    339. 
#>  8 /home/runner… s3744          2    0.004 HSO4-    17O                     66.6
#>  9 /home/runner… s3744          2    0.004 HSO4-    34S                   2315. 
#> 10 /home/runner… s3744          2    0.004 HSO4-    18O                    438. 
#> # ℹ 6,439 more rows
#> # ℹ 7 more variables: tic <dbl>, it.ms <dbl>, data_group <int>, block <int>,
#> #   block_name <chr>, data_type <chr>, segment <int>

# the same via a blocks table, here mixing time and scan definitions
df |> orbi_define_blocks(
  blocks_table = tibble::tibble(
    start_time.min = c(0.1, NA),
    end_time.min = c(0.4, NA),
    start_scan.no = c(NA, 2000),
    end_scan.no = c(NA, 2500),
    block_name = c("first", "second")
  )
)
#> Warning: ! block second: scan 2000 to 2500 is outside the data in 3 files and was not
#>   added there
#> ℹ no scans in s3744 (covers scans 1 to 430)
#> ℹ no scans in ac5 (covers scans 1 to 430)
#> ℹ no scans in ac6 (covers scans 1 to 430)
#> ✔ [115ms] orbi_define_blocks() added 1 of 2 blocks to 3 files
#> • block first: covers scans 43 to 172 (0.1 to 0.399 min) in 3 files
#> • block second: not added, outside the data in 3 files
#> # A tibble: 6,449 × 14
#>    filepath      filename scan.no time.min compound isotopocule ions.incremental
#>    <chr>         <fct>      <int>    <dbl> <fct>    <fct>                  <dbl>
#>  1 /home/runner… s3744          1    0.002 HSO4-    M0                   37803. 
#>  2 /home/runner… s3744          1    0.002 HSO4-    33S                    326. 
#>  3 /home/runner… s3744          1    0.002 HSO4-    17O                     61.9
#>  4 /home/runner… s3744          1    0.002 HSO4-    34S                   2215. 
#>  5 /home/runner… s3744          1    0.002 HSO4-    18O                    436. 
#>  6 /home/runner… s3744          2    0.004 HSO4-    M0                   42346. 
#>  7 /home/runner… s3744          2    0.004 HSO4-    33S                    339. 
#>  8 /home/runner… s3744          2    0.004 HSO4-    17O                     66.6
#>  9 /home/runner… s3744          2    0.004 HSO4-    34S                   2315. 
#> 10 /home/runner… s3744          2    0.004 HSO4-    18O                    438. 
#> # ℹ 6,439 more rows
#> # ℹ 7 more variables: tic <dbl>, it.ms <dbl>, data_group <int>, block <int>,
#> #   block_name <chr>, data_type <chr>, segment <int>
```
