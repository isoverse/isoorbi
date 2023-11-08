
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

To use the latest updates, you can install the development version of
`isoorbi` from [GitHub](https://github.com/) with:

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
```

## Package layout

<img src="man/figures/figure_flowchart.png" style="width:60.0%" />

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/isoverse/isoorbi/issues).

For questions and other discussion, please use the [isoorbi slack
workspace](https://isoorbi.slack.com).
