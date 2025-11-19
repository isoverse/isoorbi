# Visualize data

Call this function to visualize orbitrap data vs. time or scan number.
The most common uses are `orbi_plot_raw_data(y = intensity)`,
`orbi_plot_raw_data(y = ratio)`, and
`orbi_plot_raw_data(y = tic * it.ms)`. If the selected `y` is
peak-specific data (rather than scan-specific data like `tic * it.ms`),
the `isotopocules` argument can be used to narrow down which
isotopocules will be plotted. By default includes all isotopcules that
have not been previously identified by `orbi_flag_weak_isotopcules()`
(if already called on dataset).

## Usage

``` r
orbi_plot_raw_data(
  dataset,
  isotopocules = c(),
  x = c("time.min", "scan.no"),
  x_breaks = scales::breaks_pretty(5),
  y,
  y_scale = c("raw", "linear", "pseudo-log", "log"),
  y_scale_sci_labels = TRUE,
  color = .data$isotopocule,
  colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
    "#666666", "#BBBBBB"),
  color_scale = scale_color_manual(values = colors),
  add_data_blocks = TRUE,
  add_all_blocks = FALSE,
  show_outliers = TRUE
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

- y:

  expression for what to plot on the y-axis, e.g. `intensity`,
  `tic * it.ms` (pick one `isotopocules` as this is identical for
  different istopocules), `ratio`. Depending on the variable, you may
  want to adjust the `y_scale` and potentially `y_scale_sci_labels`
  argument.

- y_scale:

  what type of y scale to use: "log" scale, "pseudo-log" scale (smoothly
  transitions to linear scale around 0), "linear" scale, or "raw" (if
  you want to add a y scale to the plot manually instead)

- y_scale_sci_labels:

  whether to render numbers with scientific exponential notation

- color:

  expression for what to use for the color aesthetic, default is
  isotopocule

- colors:

  which colors to use, by default a color-blind friendly color palettes
  (RColorBrewer, dark2)

- color_scale:

  use this parameter to replace the entire color scale rather than just
  the `colors`

- add_data_blocks:

  add highlight for data blocks if there are any block definitions in
  the dataset (uses
  [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md)).
  To add blocks manually, set `add_data_blocks = FALSE` and manually
  call the
  [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md)
  function afterwards.

- add_all_blocks:

  add highlight for all blocks, not just data blocks (equivalent to the
  `data_only = FALSE` argument in
  [`orbi_add_blocks_to_plot()`](https://isoorbi.isoverse.org/reference/orbi_add_blocks_to_plot.md))

- show_outliers:

  whether to highlight data previously flagged as outliers by
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/reference/orbi_flag_outliers.md)

## Value

a ggplot object
