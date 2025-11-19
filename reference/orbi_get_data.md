# Get data frame from aggregated data

Retrieve a specific subset of the aggregated data into a single data
frame by specifying which columns to take from each dataset (file_info,
scans, peaks, etc.) using
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
syntax. If data from more than one dataset is selected (e.g. some
columns from `scans` AND some from `peaks`), the datasets are combined
with an
[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
using the columns listed in `by` (only the ones actually in the
datasets). Joins that would lead to duplicated data entries (i.e.
many-to-many joins) are not allowed and will throw an error to avoid
unexpected replications of individual datapoints. If you really want to
do such a join, you'll have to do it manually.

## Usage

``` r
orbi_get_data(
  aggregated_data,
  file_info = c("filename"),
  scans = NULL,
  peaks = NULL,
  spectra = NULL,
  problems = NULL,
  summary = NULL,
  by = c("uidx", "scan.no")
)
```

## Arguments

- aggregated_data:

  datasets aggregated from
  [`orbi_aggregate_raw()`](https://isoorbi.isoverse.org/reference/orbi_aggregate_raw.md)

- file_info:

  columns to get from the aggregated `file_info`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- scans:

  columns to get from the aggregated `scans`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- peaks:

  columns to get from the aggregated `peaks`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- spectra:

  columns to get from the aggregated `spectra`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- problems:

  columns to get from the aggregated `problems`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- summary:

  columns to get from the `summary` calculated via
  [`orbi_summarize_results()`](https://isoorbi.isoverse.org/reference/orbi_summarize_results.md),
  all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported. Warning: it is not advisable to combine columns
  from `summary` with anything other than `file_info` as it will lead to
  duplicated datasets given that `summary` integrates across multiple
  scans.

- by:

  which columns to look for when joining datasets together. Make sure to
  include the relevant `by` columns in the selections of the individual
  datasets so they are joined correctly. The default is usually
  sufficient

## Value

a tibble
