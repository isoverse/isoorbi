# Read RAW files

Read raw data files (`.raw`) from Orbitrap IRMS runs directly. This
function extracts all available information and thus can be relatively
slow (~1s / Mb on a typical personal computer) but with the caching this
is only true the first time. The results can be used directly or, more
typically, are aggregated with
[`orbi_aggregate_raw()`](https://isoorbi.isoverse.org/reference/orbi_aggregate_raw.md)
to safely extract the relevant information for downstream processing.
This function is designed to be fail save by safely catching errors and
reporting back on them (see
[`orbi_get_problems()`](https://isoorbi.isoverse.org/reference/problems.md)).

## Usage

``` r
orbi_read_raw(
  file_paths,
  show_progress = rlang::is_interactive(),
  show_problems = TRUE,
  include_spectra = FALSE,
  read_cache = TRUE,
  cache = TRUE,
  cache_spectra = cache,
  keep_cached_spectra = cache
)
```

## Arguments

- file_paths:

  paths to the `.raw` file(s), single value or vector of paths. Use
  [`orbi_find_raw()`](https://isoorbi.isoverse.org/reference/orbi_find_raw.md)
  to get all raw files in a folder.

- show_progress:

  whether to show a progress bar, by default always enabled when running
  interactively e.g. inside RStudio (and disabled in a notebook), turn
  off with `show_progress = FALSE`

- show_problems:

  whether to show problems encountered along the way (rather than just
  keeping track of them with
  [`orbi_get_problems()`](https://isoorbi.isoverse.org/reference/problems.md)).
  Set to `show_problems = FALSE` to turn off the live printout. Either
  way, all encountered problems can be retrieved with running
  [`orbi_get_problems()`](https://isoorbi.isoverse.org/reference/problems.md)
  for the returned list

- include_spectra:

  whether to include the spectral data from specific scans (e.g.
  `include_spectra = c(5, 100, 200)` reads out the spectra from scans 5,
  100, and 200 for each file if they exist) or from all scans
  (`include_spectra = TRUE`). Including many or all scan spectra makes
  the read process slower (especially if `cache_spectra = FALSE`) and
  the returned data frame tibble significantely larger. The default is
  `FALSE` (i.e. scan spectra are not returned).

- read_cache:

  whether to read the file from cached .parquet files (if they exist) or
  anew

- cache:

  whether to automatically cache the read raw files (writes highly
  efficient .parquet files in a folder with the same name as the file
  .cache appended)

- cache_spectra:

  whether to automatically cache requested scan spectra (this can take
  up significant disc space), by default the same as `cache`

- keep_cached_spectra:

  whether to keep the spectra from a raw file that were previously
  cached whenever `include_spectra` changes and requires reading the
  file anew. Having this TRUE (the default) makes it faster to iterate
  on code that changes which spectra to read but leads to larger cache
  files.

## Value

a tibble data frame where each row holds the file path and nested
tibbles of datasets extracted from the raw file (typically `file_info`,
`scans`, `peaks`, and `spectra`). This is the safest way to extract the
data without needing to make assumptions about compatibility across
files. Extract your data of interest from the tibble columns or use
[`orbi_aggregate_raw()`](https://isoorbi.isoverse.org/reference/orbi_aggregate_raw.md)
to extract safely across files.
