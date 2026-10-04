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
  x_breaks = NULL,
  n_x_breaks = 5,
  short_time_labels = FALSE,
  add_data_blocks = TRUE,
  colors = c("#7570B3", "#E6AB02", "#66A61E", "#A6761D", "#D95F02", "#1B9E77", "#E7298A",
    "#666666", "#BBBBBB")
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

  what breaks to use for the x axis. By default (`NULL`) these are
  pretty breaks for scan numbers or pretty time intervals for time
  (which is labeled as a duration, e.g. `1:30 min`), provide breaks to
  make more specific tickmarks. Use either `x_breaks` or `n_x_breaks`,
  not both.

- n_x_breaks:

  the desired number of x axis breaks when using the default pretty
  breaks (`x_breaks = NULL`), default: `5`. Use either `x_breaks` or
  `n_x_breaks`, not both.

- short_time_labels:

  whether to use compact time axis labels with no space between value
  and unit and abbreviated units (`hr`, `m`, `s`), e.g. `1:30m` instead
  of `1:30 min`. Only relevant for a time based x axis
  (`x = "time.min"`).

- add_data_blocks:

  add highlight for data blocks if there are any block definitions in
  the dataset (uses
  [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md)).
  To add blocks manually, set `add_data_blocks = FALSE` and manually
  call the
  [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md)
  function afterwards.

- colors:

  the fill colors for the isotopocules that carry peak flags, one per
  flag combination encountered in the data (recycled if there are more
  combinations than colors). Isotopocules without any flags are always
  shown in black.

## Value

a ggplot object

summary data frame

## Functions

- `orbi_plot_isotopocule_coverage()`: visualizes isotope coverage.
  Detected isotopocules are shown by their peak flags - those without
  any flags in black, those carrying flags in the `colors` provided.
  Weak isotopocules (if previously defined by
  [`orbi_flag_weak_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_flag_weak_isotopocules.md))
  are highlighted in red.

- `orbi_get_isotopocule_coverage()`: calculates which stretches of the
  data have data for which isotopocules. This function is usually used
  indicrectly by `orbi_plot_isotopocule_coverage()` but can be called
  directly to investigate isotopocule coverage.
