# Make a shot noise plot

This function creates a shot noise plot using a `shotnoise` data frame
created by the
[`orbi_analyze_shot_noise()`](https://isoorbi.isoverse.org/reference/orbi_analyze_shot_noise.md)
function.

## Usage

``` r
orbi_plot_shot_noise(
  shotnoise,
  x = c("time.min", "n_effective_ions"),
  permil_target = NA_real_,
  color = "ratio_label",
  colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
    "#666666")
)
```

## Arguments

- shotnoise:

  a `shotnoise` data frame

- x:

  x-axis for the shot noise plot, either "time.min" or
  "n_effective_ions"

- permil_target:

  highlight the target permil in the shotnoise plot

- color:

  which column to use for the color aesthetic (must be a factor)

- colors:

  which colors to use, by default a color-blind friendly color palettes
  (RColorBrewer, dark2)

## Value

a ggplot object

## Details

plot shot noise
