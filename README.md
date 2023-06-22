# isoorbi <a href='https://isoorbi.isoverse.org/'> <img src="inst/www/logo.png" align="right" height="138" /> </a>

<!-- badges: start -->
  [![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://isoorbi.isoverse.org/)
  [![R-CMD-check](https://github.com/isoverse/isoorbi/workflows/R-CMD-check/badge.svg)](https://github.com/isoverse/isoorbi/actions)
  [![codecov](https://codecov.io/gh/isoverse/isoorbi/branch/main/graph/badge.svg?token=SN0YDIJ6Y6)](https://app.codecov.io/gh/isoverse/isoorbi)
[![CRAN status](https://www.r-pkg.org/badges/version/isoorbi)](https://CRAN.R-project.org/package=isoorbi)
<!-- badges: end -->

## Overview

The goal of the isoorbi R package is to help you process isotopocule measurements from an **Orbitrap Isotope Solutions** mass spectrometer. It expects <code>.isox</code> files created by IsoX as input.

## Installation

``` r
# To install from CRAN
install.packages("isoorbi")
```

## Getting started

``` r
library(isoorbi)

orbi_read_isox(file)
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/isoverse/isoorbi/issues). 

For questions and other discussion, please use the [isoorbi slack workspace](https://isoorbi.slack.com).

## Development version

To get a bug fix or to use the latest features, install the development version of isoorbi from GitHub.

``` r
# install the development version 
install.packages("devtools")

devtools::install_github("isoverse/isoorbi")
```
