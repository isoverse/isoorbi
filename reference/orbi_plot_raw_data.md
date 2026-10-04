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
  x_breaks = NULL,
  n_x_breaks = 5,
  short_time_labels = FALSE,
  y,
  y_scale = c("raw", "linear", "pseudo-log", "log"),
  y_scale_sci_labels = TRUE,
  color = .data$isotopocule,
  colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
    "#666666", "#BBBBBB"),
  color_scale = scale_color_manual(values = colors),
  add_data_blocks = TRUE,
  add_all_blocks = FALSE,
  use_data_block_names = FALSE,
  show_outliers = TRUE,
  show_points = FALSE,
  point_size = NULL
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

  which x-axis to use (time vs. scan number). If set to "guess" (the
  default), the function will try to figure it out from the plot.

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

- use_data_block_names:

  whether to label the data blocks by their individual `block_name` (if
  they have one) instead of just as "data" (the default). This allows
  color coding the background of the different data blocks (e.g.
  reference vs. sample). All other blocks (e.g. "unused") are always
  labeled by their data type.

- show_outliers:

  whether to highlight data previously flagged as outliers by
  [`orbi_flag_outliers()`](https://isoorbi.isoverse.org/reference/orbi_flag_outliers.md)

- show_points:

  whether to show the individual data points in addition to the lines
  connecting them

- point_size:

  the size of the data points (if `show_points = TRUE`) and of the
  outlier points (if `show_outliers = TRUE`). By default (`NULL`) the
  ggplot2 default point size is used.

## Value

a ggplot object
