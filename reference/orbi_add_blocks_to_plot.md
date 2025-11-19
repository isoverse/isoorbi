# Plot blocks background

This function can be used to add colored background to a plot of
dual-inlet data where different colors signify different data types
(data, startup time, changeover time, unused). Note that this function
only works with continuous and pseudo-log y axis, not with log y axes.

## Usage

``` r
orbi_add_blocks_to_plot(
  plot,
  x = c("guess", "scan.no", "time.min"),
  data_only = FALSE,
  fill = .data$data_type,
  fill_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
    "#A6761D", "#666666"),
  fill_scale = scale_fill_manual("blocks", values = fill_colors),
  alpha = 0.5,
  show.legend = !data_only
)
```

## Arguments

- plot:

  object with a dataset that has defined blocks

- x:

  which x-axis to use (time vs. scan number). If set to "guess" (the
  default), the function will try to figure it out from the plot.

- data_only:

  if set to TRUE, only the blocks flagged as "data"
  (`orbi_get_option("data_type_data")`) are highlighted

- fill:

  what to use for the fill aesthetic, default is the block `data_type`

- fill_colors:

  which colors to use, by default a color-blind friendly color palettes
  (RColorBrewer, dark2)

- fill_scale:

  use this parameter to replace the entire fill scale rather than just
  the `fill_colors`

- alpha:

  opacity settings for the background

- show.legend:

  whether to include the background information in the legend
