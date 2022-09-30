# isoorbi

# isoorbi <img src="man/figures/logo.svg" align="right" height="139" />

<!-- badges: start -->
  [![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://www.isoverse.org/isoorbi)
  [![R-CMD-check](https://github.com/isoverse/isoorbi/workflows/R-CMD-check/badge.svg)](https://github.com/isoverse/isoorbi/actions)
<!-- badges: end -->

## About

This R package is providing functions for processing isotopocule measurements from an Orbitrap mass spectrometer. It uses <code>.isox</code> files created by IsoX as input.

## Installation

To install from CRAN:

```
install.packages("isoorbi")
```

## Usage

```
library(isoorbi)

orbi_read_isox(filepath)
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/isoverse/isoorbi/issues). 

## Development version

To get a bug fix or to use the latest features from the development version, you can install 
the development version of isoorbi from GitHub.

```
install.packages("devtools")

devtools::install_github("isoverse/isoorbi")
```
