
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isoorbi <a href='https://isoorbi.isoverse.org/'> <img src="inst/www/logo.png" align="right" height="138" /> </a>

<!-- badges: start -->

[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://isoorbi.isoverse.org/)
[![R-CMD-check](https://github.com/isoverse/isoorbi/workflows/R-CMD-check/badge.svg)](https://github.com/isoverse/isoorbi/actions)
[![codecov](https://codecov.io/gh/isoverse/isoorbi/branch/main/graph/badge.svg?token=SN0YDIJ6Y6)](https://app.codecov.io/gh/isoverse/isoorbi)
[![CRAN
status](https://www.r-pkg.org/badges/version/isoorbi)](https://CRAN.R-project.org/package=isoorbi)
<!-- badges: end -->

## Overview

The goal of the isoorbi R package is to help you process isotopocule
measurements from an **Orbitrap Isotope Solutions** mass spectrometer.
It expects <code>.isox</code> files created by IsoX as input.

## Installation

You can install the current CRAN version of `isoorbi` with:

``` r
install.packages("isoorbi")
```

To get a bug fix or to use the latest features, you can install the
latest development version of `isoorbi` from
[GitHub](https://github.com/) with:

``` r
if(!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("isoverse/isoorbi")
```

## Show me some code

``` r
library(isoorbi)

system.file(package = "isoorbi", "extdata", "testfile_flow.isox") |>
  orbi_read_isox() |>
  orbi_flag_satellite_peaks() |>
  orbi_define_basepeak(basepeak_def = "M0")|> 
  orbi_summarize_results(ratio_method = "sum") |>
  orbi_export_data_to_excel(file = "data_summary.xlsx")
#> orbi_read_isox() is loading .isox data from 1 file(s)...
#> - loaded 6449 peaks for 1 compounds (HSO4-) with 5 isotopocules (M0, 33S,
#>    17O, 34S, 18O) from testfile_flow.isox in 0.22 seconds.
#> orbi_flag_satellite_peaks() is flagging minor signals (satellite peaks)...
#>    ...confirmed there are no satellite peaks in 0.20 seconds.
#> orbi_define_basepeak() is setting the 'M0' isotopocule as the ratio
#>    denominator...
#>    ...set base peak and calculated 5159 ratios for 4 isotopocules/base peak
#>    (33S, 17O, 34S, 18O) in 0.11 seconds.
#> orbi_summarize_results() is grouping the data by 'filename', 'compound',
#>    'basepeak', 'isotopocule' and summarizing ratios from 5159 peaks (excluded
#>    0 flagged peaks; excluded 0 unused peaks) using the 'sum' method...
#>    ...completed in 0.02 seconds.
#> orbi_export_data_to_excel() is exporting data set with 12 rows and 15 columns
#>    to data_summary.xlsx...
#>    ...completed in 0.25 seconds.
```

## Package layout

![](figure_flowchart.png)

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/isoverse/isoorbi/issues).

For questions and other discussion, please use the [isoorbi slack
workspace](https://isoorbi.slack.com).
