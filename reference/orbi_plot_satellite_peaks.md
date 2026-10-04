# Visualize satellite peaks

Call this function any time after flagging the satellite peaks to see
where they are. Use the `isotopocules` argument to focus on the specific
isotopocules of interest.

## Usage

``` r
orbi_plot_satellite_peaks(
  dataset,
  isotopocules = c(),
  x = c("scan.no", "time.min"),
  y = c("ions.incremental", "intensity"),
  x_breaks = NULL,
  n_x_breaks = 5,
  short_time_labels = FALSE,
  y_scale = c("log", "pseudo-log", "linear", "raw"),
  y_scale_sci_labels = TRUE,
  colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
    "#666666", "#BBBBBB"),
  color_scale = scale_color_manual(values = colors)
)
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

- y:

  y-axis column for the plot, typially either "ions.incremental" or
  "intensity", default is "ions.incremental" (falls back to "intensity"
  if "ions.incremental" has not been calculated yet for the provided
  dataset)

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

- y_scale:

  what type of y scale to use: "log" scale, "pseudo-log" scale (smoothly
  transitions to linear scale around 0), "linear" scale, or "raw" (if
  you want to add a y scale to the plot manually instead)

- y_scale_sci_labels:

  whether to render numbers with scientific exponential notation

- colors:

  which colors to use, by default a color-blind friendly color palettes
  (RColorBrewer, dark2)

- color_scale:

  use this parameter to replace the entire color scale rather than just
  the `colors`

## Value

a ggplot object
