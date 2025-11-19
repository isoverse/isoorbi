# Aggregate data from raw files

This function allows dynamic aggregation and validation of data read by
[`orbi_read_raw()`](https://isoorbi.isoverse.org/reference/orbi_read_raw.md).
Like
[`orbi_read_raw()`](https://isoorbi.isoverse.org/reference/orbi_read_raw.md),
it is designed to be fail save by safely catching errors and reporting
back on them (see
[`orbi_get_problems()`](https://isoorbi.isoverse.org/reference/problems.md)).
This function should work out of the box for most files without
additional modification of the `aggregator`.

## Usage

``` r
orbi_aggregate_raw(
  files_data,
  aggregator = "standard",
  show_progress = rlang::is_interactive(),
  show_problems = TRUE
)
```

## Arguments

- files_data:

  the files read in by
  [`orbi_read_raw()`](https://isoorbi.isoverse.org/reference/orbi_read_raw.md)

- aggregator:

  typically the name of a registered aggregator (see all with
  `orbi_get_option("aggregators")`), default is the "standard"
  aggregator included in the package
  (`orbi_get_aggregator("standard")`). Other options are "minimal"
  (`orbi_get_aggregator("minimal")`) and "extended"
  (`orbi_get_aggregator("extended")`). The `aggregator` parameter can
  can also directly be an aggregator tibble (created/modified with
  [`orbi_start_aggregator()`](https://isoorbi.isoverse.org/reference/orbi_aggregator.md)
  and/or
  [`orbi_add_to_aggregator()`](https://isoorbi.isoverse.org/reference/orbi_aggregator.md))
  that defines which data should be aggregated and how.

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

## Value

a list of merged dataframes collected from the `files_data` based on the
`aggregator` definitions
