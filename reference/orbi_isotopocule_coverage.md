# Isotopocule coverage

The coverage of each isotopcule across scans/time is an important
indicator for data completeness. These functions provide ways to
summarize and visualize the isotopocule coverage in a dataset.

## Usage

``` r
orbi_plot_isotopocule_coverage(
  dataset,
  isotopocules = c(),
  x = c("scan.no", "time.min"),
  x_breaks = scales::breaks_pretty(5),
  add_data_blocks = TRUE
)

orbi_get_isotopocule_coverage(dataset)
```

## Arguments

- dataset:

  a data frame or aggregated dataset with satellite peaks already
  identified (i.e. after
  [`orbi_flag_satellite_peaks()`](https://isoorbi.isoverse.org/reference/orbi_flag_satellite_peaks.md))

- isotopocules:

  which isotopocules to visualize, if none provided will visualize all
  (this may take a long time or even crash your R session if there are
  too many isotopocules in the data set)

- x:

  x-axis column for the plot, either "time.min" or "scan.no", default is
  "scan.no"

- x_breaks:

  what breaks to use for the x axis, change to make more specifid
  tickmarks

- add_data_blocks:

  add highlight for data blocks if there are any block definitions in
  the dataset (uses
  [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md)).
  To add blocks manually, set `add_data_blocks = FALSE` and manually
  call the
  [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md)
  function afterwards.

## Value

a ggplot object

summary data frame

## Functions

- `orbi_plot_isotopocule_coverage()`: visualizes isotope coverage. Weak
  isotopocules (if previously defined by
  [`orbi_flag_weak_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_flag_weak_isotopocules.md))
  are highlighted in red.

- `orbi_get_isotopocule_coverage()`: calculates which stretches of the
  data have data for which isotopocules. This function is usually used
  indicrectly by `orbi_plot_isotopocule_coverage()` but can be called
  directly to investigate isotopocule coverage.
