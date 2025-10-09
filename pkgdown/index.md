<!-- README.md is generated from README.Rmd. Please edit that file -->

# isoorbi <a href='https://isoorbi.isoverse.org/'> <img src="inst/www/logo.png" align="right" height="138" /> </a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/isoorbi)](https://CRAN.R-project.org/package=isoorbi)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://isoorbi.isoverse.org/)
[![R-CMD-check](https://github.com/isoverse/isoorbi/workflows/R-CMD-check/badge.svg)](https://github.com/isoverse/isoorbi/actions)
[![Codecov test
coverage](https://codecov.io/gh/isoverse/isoorbi/graph/badge.svg)](https://app.codecov.io/gh/isoverse/isoorbi)
<!-- badges: end -->

## Overview

The goal of the isoorbi R package is to help you process isotopocule
measurements from an **Orbitrap Isotope Solutions** mass spectrometer.
It can read both the <code>.raw</code> files (recommended approach) as
well as <code>.isox</code> output created by IsoX (legacy approach).

## Installation

You can install the current CRAN version of `isoorbi` with:

    # install the isoorbi package
    install.packages("isoorbi")
    # check/install the isoraw reader
    isoorbi::orbi_check_isoraw()

To use the latest updates, you can install the development version of
`isoorbi` from [GitHub](https://github.com/). If you are on Windows,
make sure to install the equivalent version of
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) for your
version of R (you can find out which version you have with
`getRversion()` from an R console - note that isoorbi requires [R
version](https://cran.r-project.org/) 4.4 or newer).

    # checks that you are set up to build R packages from source
    if(!requireNamespace("pkgbuild", quietly = TRUE)) install.packages("pkgbuild")
    pkgbuild::check_build_tools()

    # installs the latest isoorbi package from GitHub
    if(!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
    pak::pak("isoverse/isoorbi")

    # check/install the isoraw reader
    isoorbi::orbi_check_isoraw()

> Important: as of isoorbi version 1.5.0, it is possible to read .raw
> files directly using the [isoraw
> reader](https://github.com/isoverse/isoorbi/tree/main/inst/assembly)
> built into this package. The first time you read a .raw file, you will
> be asked to agree to [Thermo’s license
> agreement](https://github.com/fgcz/rawrr/blob/devel/inst/rawrrassembly/RawFileReaderLicense.txt)
> to proceed. Implementation of the isoraw reader, would not have been
> possible without the example provided by Jim Shofstahl as part of
> Thermo’s
> [RawFileReader](https://github.com/thermofisherlsms/RawFileReader) and
> the raw file reader developed by Witold Wolski, Christian Panse,
> Christian Trachsel, and Tobias Kockmann as part of the [rawrr
> package](https://github.com/fgcz/rawrr).

## Show me some code

### Read raw data file

    # load library
    library(isoorbi)

    # provide the path to your data folder here:
    my_data_folder <- file.path("project", "data")

    # and search for raw files in that folder
    file_paths <- orbi_find_raw(my_data_folder)

    # for this example, we use a small raw test file bundled with the
    # package instead (remove this line if working with your own data)
    file_paths <- orbi_get_example_files("nitrate_test_10scans.raw")

    # read the raw file incluing 2 of the raw spectra
    raw_files <- file_paths |>
        orbi_read_raw(include_spectra = c(1, 10))

    # aggregate the raw data (processes the read files)
    raw_agg <- raw_files |> orbi_aggregate_raw()

    # plot the spectra
    raw_agg |> orbi_plot_spectra()

<img src="man/figures/README-read-raw-1.png" width="100%" />

### Identify isotopcules

    # identify isotopcules
    # these could also come from a data frame or a tsv/csv/excel file
    raw_agg <- raw_agg |> orbi_identify_isotopocules(
      isotopocules = 
        c("M0" = 61.9878, "15N" = 62.9850, "17O" = 62.9922, "18O" = 63.9922)
    )

    # plot again, now with the isotopocules identified
    raw_agg |> orbi_plot_spectra()

<img src="man/figures/README-map-isotopocules-1.png" width="100%" />

### Process data

    # process raw files data
    dataset <- raw_agg |>
      # filter out unidentified peaks
      orbi_filter_isotopocules() |>
      # check for satellite peaks
      orbi_flag_satellite_peaks() |>
      # define base peak
      orbi_define_basepeak(basepeak_def = "M0")

    # plot the resulting isotopocule ratios
    dataset |> orbi_plot_raw_data(y = ratio)

<img src="man/figures/README-process-data-1.png" width="100%" />

### Summarize results

    # calculate ratios across scans
    results <- dataset |> orbi_summarize_results(ratio_method = "sum")
       
    # print results
    results |>  orbi_get_data(summary = c("isotopocule", "ratio", "ratio_sem"))

    # export results to excel
    results |> orbi_export_data_to_excel(
      file = "data_summary.xlsx",
      include = c("file_info", "summary")
    )

<PRE class="fansi fansi-output"><CODE><span style='color: #949494;'># A tibble: 3 × 5</span>
   uidx filename             isotopocule   ratio ratio_sem
  <span style='color: #949494; font-style: italic;'>&lt;int&gt;</span> <span style='color: #949494; font-style: italic;'>&lt;chr&gt;</span>                <span style='color: #949494; font-style: italic;'>&lt;fct&gt;</span>         <span style='color: #949494; font-style: italic;'>&lt;dbl&gt;</span>     <span style='color: #949494; font-style: italic;'>&lt;dbl&gt;</span>
<span style='color: #BCBCBC;'>1</span>     1 nitrate_test_10scans 15N         0.004<span style='text-decoration: underline;'>22</span> 0.000<span style='text-decoration: underline;'>098</span>0
<span style='color: #BCBCBC;'>2</span>     1 nitrate_test_10scans 17O         0.001<span style='text-decoration: underline;'>32</span> 0.000<span style='text-decoration: underline;'>055</span>4
<span style='color: #BCBCBC;'>3</span>     1 nitrate_test_10scans 18O         0.007<span style='text-decoration: underline;'>75</span> 0.000<span style='text-decoration: underline;'>162</span> 
</CODE></PRE>

For additional code, please check out our **Examples** in the main menu
at [isoorbi.isoverse.org](https://isoorbi.isoverse.org/), and peruse the
full package structure below.

## Package structure

<p>
Click on the individual functions to jump straight to their
documenation.
</p>

    Warning in readLines("man/figures/figure_flowchart.svg"): incomplete final line
    found on 'man/figures/figure_flowchart.svg'

<?xml version="1.0" encoding="UTF-8"?>
<svg id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="0 0 834.54 1349.22">
<metadata><c2pa:manifest xmlns:c2pa="http://c2pa.org/manifest">AAA0RWp1bWIAAAAeanVtZGMycGEAEQAQgAAAqgA4m3EDYzJwYQAAADQfanVtYgAAAEdqdW1kYzJtYQARABCAAACqADibcQN1cm46dXVpZDozZWYzZTQwMS1mNGJjLTRhMGQtODYzNy00OGZhMDNlZjYwYTQAAAABtGp1bWIAAAApanVtZGMyYXMAEQAQgAAAqgA4m3EDYzJwYS5hc3NlcnRpb25zAAAAANdqdW1iAAAAJmp1bWRjYm9yABEAEIAAAKoAOJtxA2MycGEuYWN0aW9ucwAAAACpY2JvcqFnYWN0aW9uc4GjZmFjdGlvbmtjMnBhLmVkaXRlZG1zb2Z0d2FyZUFnZW50bUFkb2JlIEZpcmVmbHlxZGlnaXRhbFNvdXJjZVR5cGV4U2h0dHA6Ly9jdi5pcHRjLm9yZy9uZXdzY29kZXMvZGlnaXRhbHNvdXJjZXR5cGUvY29tcG9zaXRlV2l0aFRyYWluZWRBbGdvcml0aG1pY01lZGlhAAAArGp1bWIAAAAoanVtZGNib3IAEQAQgAAAqgA4m3EDYzJwYS5oYXNoLmRhdGEAAAAAfGNib3KlamV4Y2x1c2lvbnOBomVzdGFydBjxZmxlbmd0aBlFtGRuYW1lbmp1bWJmIG1hbmlmZXN0Y2FsZ2ZzaGEyNTZkaGFzaFgg8hra46Kr3/YRSWDi9OHweghuZ8m3x7vMo4Jzf2EYlzljcGFkSQAAAAAAAAAAAAAAAgxqdW1iAAAAJGp1bWRjMmNsABEAEIAAAKoAOJtxA2MycGEuY2xhaW0AAAAB4GNib3KoaGRjOnRpdGxlb0dlbmVyYXRlZCBJbWFnZWlkYzpmb3JtYXRtaW1hZ2Uvc3ZnK3htbGppbnN0YW5jZUlEeCx4bXA6aWlkOmI2YTMwYTA3LTc3Y2ItNDAxZi05YzQ5LWQwMzAzZjdlODhlNG9jbGFpbV9nZW5lcmF0b3J4N0Fkb2JlX0lsbHVzdHJhdG9yLzI5LjggYWRvYmVfYzJwYS8wLjEyLjIgYzJwYS1ycy8wLjMyLjV0Y2xhaW1fZ2VuZXJhdG9yX2luZm+Bv2RuYW1lcUFkb2JlIElsbHVzdHJhdG9yZ3ZlcnNpb25kMjkuOP9pc2lnbmF0dXJleBlzZWxmI2p1bWJmPWMycGEuc2lnbmF0dXJlamFzc2VydGlvbnOComN1cmx4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AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD2WQEAdZd2hc7yj/+3gGB+he4W3EGGJOqqXtEY1NiTPuin+l9c3AqQpe5CiwQVKRPJwNvxVndzKrOJ9PkrOpJyzuxMUmmhg8f8LM+Geq8KuLayYQiVM1jMG4C5VVDGW1WSvtkk/5+5c492Dhm7DKpNxkm18yvPE7JwYE7tRXXG6oU+Juv31WtNqo0zluFLAZPbbA/vR1etCSws5pdjTzM8EUkAUPFW7Mugq6XsthbpJIZpCljrN82wA5NnK0NgVadNINS3qheUIUohRrWLJ16HFadcD6aDKsCp0LrAU8vJOnLxLOy1S2tgTPQxRG6hUKKM/nSqxY+EBDowzUj+pb3FJh/iEA==</c2pa:manifest></metadata>
<!-- Generator: Adobe Illustrator 29.8.1, SVG Export Plug-In . SVG Version: 2.1.1 Build 2)  -->
<defs>
<style>
      .st0, .st1, .st2, .st3, .st4, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st12, .st13, .st14, .st15, .st16, .st17, .st18, .st19, .st20, .st21, .st22, .st23, .st24, .st25, .st26, .st27, .st28 {
        fill: none;
      }

      .st29 {
        fill: #010101;
      }

      .st1, .st2, .st30, .st31, .st3, .st4, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st12, .st13, .st14, .st15, .st16, .st17, .st18, .st19, .st20, .st21, .st22, .st23, .st24, .st25, .st26, .st27, .st28 {
        stroke-miterlimit: 10;
      }

      .st1, .st2, .st30, .st31, .st4, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st13, .st14, .st15, .st16, .st18, .st24, .st25, .st26, .st28 {
        stroke: #231f20;
      }

      .st1, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st12, .st17, .st18, .st26, .st28 {
        stroke-width: 2px;
      }

      .st1, .st12, .st24 {
        stroke-dasharray: 6 6;
      }

      .st2, .st3 {
        stroke-dasharray: 6;
        stroke-width: 2.36px;
      }

      .st32, .st33, .st34 {
        fill: #231f20;
      }

      .st35 {
        fill: #e2e2e2;
      }

      .st36 {
        fill: #fbb042;
      }

      .st30 {
        stroke-width: 1.53px;
      }

      .st30, .st31 {
        fill: #cdacd1;
      }

      .st31, .st22 {
        stroke-width: 2.66px;
      }

      .st3, .st12, .st17, .st20, .st21, .st22, .st23, .st27 {
        stroke: #878787;
      }

      .st4 {
        stroke-width: 2.15px;
      }

      .st5 {
        stroke-dasharray: 6.17 6.17;
      }

      .st37 {
        font-size: 9px;
      }

      .st37, .st33, .st38, .st39, .st34 {
        isolation: isolate;
      }

      .st37, .st39 {
        font-family: Arial-BoldMT, Arial;
        font-weight: 700;
      }

      .st33 {
        font-size: 19.94px;
      }

      .st33, .st34 {
        font-family: ArialMT, Arial;
      }

      .st7 {
        stroke-dasharray: 8.62;
      }

      .st8 {
        stroke-dasharray: 8.23;
      }

      .st9 {
        stroke-dasharray: 8.01;
      }

      .st10 {
        stroke-dasharray: 7.95;
      }

      .st11 {
        stroke-dasharray: 7.75;
      }

      .st39 {
        font-size: 8px;
      }

      .st13, .st27 {
        stroke-width: 3px;
      }

      .st14 {
        stroke-width: 2.49px;
      }

      .st15, .st23 {
        stroke-width: 4px;
      }

      .st16 {
        stroke-dasharray: 7.26;
        stroke-width: 2.1px;
      }

      .st18 {
        stroke-dasharray: 6.98;
      }

      .st40 {
        fill: #878787;
      }

      .st19 {
        stroke: #000;
        stroke-width: .2px;
      }

      .st20, .st24, .st25 {
        stroke-width: 2.46px;
      }

      .st20, .st26 {
        stroke-dasharray: 5.93 5.93;
      }

      .st34 {
        font-size: 17px;
      }

      .st41 {
        fill: #5ebb55;
      }

      .st21 {
        stroke-width: 2.64px;
      }

      .st42 {
        fill: #5f88c6;
      }

      .st25 {
        stroke-dasharray: 5.95 5.95;
      }

      .st28 {
        stroke-dasharray: 7.04 7.04;
      }
    </style>
</defs> <g>
<polyline class="st17" points="288.92 107.47 288.92 123.98 261.62 123.98"/>
<polygon class="st40" points="264.54 114.01 247.27 123.98 264.54 133.95 264.54 114.01"/>
</g> <g>
<polyline class="st23" points="200.41 322.41 323.01 459.88 374.69 459.88"/>
<polygon class="st40" points="371.18 471.85 391.91 459.88 371.18 447.91 371.18 471.85"/>
</g> <g>
<line class="st23" x1="192.26" y1="214.24" x2="192.26" y2="279.64"/>
<polygon class="st40" points="180.29 276.13 192.26 296.86 204.23 276.13 180.29 276.13"/>
</g> <g>
<line class="st23" x1="126.51" y1="125.65" x2="126.51" y2="148.48"/>
<polygon class="st40" points="114.54 144.97 126.51 165.7 138.48 144.97 114.54 144.97"/>
</g> <g>
<line class="st23" x1="231.95" y1="46.17" x2="200.41" y2="72.26"/>
<polygon class="st40" points="195.34 60.86 187.28 83.41 210.83 79.11 195.34 60.86"/>
</g> <g>
<line class="st15" x1="359.41" y1="26.27" x2="385.75" y2="52.61"/>
<polygon class="st32" points="374.8 58.59 397.93 64.79 391.73 41.66 374.8 58.59"/>
</g> <g> <g>
<rect class="st41" x="83.55" y="302.3" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st27" x="83.55" y="302.3" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M97.87,323.26l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(128.39 327.8)"><tspan x="0" y="0">orbi\_read\_isox</tspan></text>
</g> </g> </g> </g> <g> <g>
<rect class="st41" x="495.58" y="365.38" width="160.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st9" x="495.58" y="365.38" width="160.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M509.9,386.34l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(534.44 390.87)"><tspan x="0" y="0">orbi\_filter\_files</tspan></text>
</g> </g> </g> </g> <g> <g>
<rect class="st41" x="8.7" y="836.79" width="302.37" height="41.92" rx="5.6" ry="5.6"/>
<rect class="st10" x="8.7" y="836.79" width="302.37" height="41.92" rx="5.6" ry="5.6"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(53.55 862.29)"><tspan x="0" y="0">orbi\_plot\_raw\_data(y
= intensity)</tspan></text> </g> </g> </g>
<path class="st29" d="M22.03,847.77c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM43.17,852.27c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g> <g>
<rect class="st41" x="296.55" y="563" width="221.67" height="41.92" rx="5.03" ry="5.03"/>
<rect class="st13" x="296.55" y="563" width="221.67" height="41.92" rx="5.03" ry="5.03"/>
</g>
<path class="st29" d="M303.46,584.46l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(324.79 589)"><tspan x="0" y="0">orbi\_flag\_satellite\_peaks</tspan></text>
</g> </g> </g> </g> <g> <g>
<rect class="st41" x="495.49" y="831.83" width="272" height="41.92" rx="5.31" ry="5.31"/>
<rect class="st11" x="495.49" y="831.83" width="272" height="41.92" rx="5.31" ry="5.31"/>
</g>
<path class="st29" d="M506.8,852.79l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(533.34 857.33)"><tspan x="0" y="0">orbi\_flag\_weak\_isotopocules</tspan></text>
</g> </g> </g> </g> <g>
<line class="st15" x1="398.56" y1="260.11" x2="398.56" y2="540.23"/>
<polygon class="st32" points="386.6 536.73 398.56 557.45 410.53 536.73 386.6 536.73"/>
</g> <g>
<line class="st15" x1="398.71" y1="755.66" x2="398.71" y2="888.7"/>
<polygon class="st32" points="386.75 885.19 398.71 905.92 410.68 885.19 386.75 885.19"/>
</g> <g>
<line class="st15" x1="399.73" y1="1190.48" x2="399.73" y2="1205.56"/>
<polygon class="st32" points="387.76 1202.06 399.73 1222.78 411.7 1202.06 387.76 1202.06"/>
</g> <g>
<line class="st15" x1="399.73" y1="1261.55" x2="399.73" y2="1279.64"/>
<polygon class="st32" points="387.76 1276.14 399.73 1296.86 411.7 1276.14 387.76 1276.14"/>
</g>
<polyline class="st15" points="374.15 604.92 374.15 757.51 401.45 757.51 424.28 757.51 424.28 709.85"/>
<g> <line class="st15" x1="424.54" y1="604.92" x2="424.54" y2="616.47"/>
<polygon class="st32" points="412.58 612.97 424.54 633.7 436.51 612.97 412.58 612.97"/>
</g> <g>
<line class="st15" x1="449.72" y1="103.57" x2="449.72" y2="119.12"/>
<polygon class="st32" points="437.76 115.62 449.72 136.35 461.69 115.62 437.76 115.62"/>
</g> <g>
<line class="st15" x1="449.72" y1="183.73" x2="449.72" y2="195.28"/>
<polygon class="st32" points="437.76 191.78 449.72 212.5 461.69 191.78 437.76 191.78"/>
</g> <g>
<line class="st1" x1="411.6" y1="426.09" x2="468.13" y2="426.09"/>
<polygon class="st32" points="465.21 436.06 482.49 426.09 465.21 416.12 465.21 436.06"/>
</g> <g>
<line class="st1" x1="476.77" y1="441.54" x2="420.25" y2="441.54"/>
<polygon class="st32" points="423.15 431.57 405.89 441.54 423.15 451.51 423.15 431.57"/>
</g> <g>
<line class="st1" x1="411.6" y1="379.11" x2="468.13" y2="379.11"/>
<polygon class="st32" points="465.21 389.08 482.49 379.11 465.21 369.14 465.21 389.08"/>
</g> <g>
<line class="st1" x1="476.77" y1="394.56" x2="420.25" y2="394.56"/>
<polygon class="st32" points="423.15 384.59 405.89 394.56 423.15 404.53 423.15 384.59"/>
</g> <g>
<line class="st1" x1="411.6" y1="333.38" x2="468.13" y2="333.38"/>
<polygon class="st32" points="465.21 343.35 482.49 333.38 465.21 323.41 465.21 343.35"/>
</g> <g>
<line class="st1" x1="476.77" y1="348.83" x2="420.25" y2="348.83"/>
<polygon class="st32" points="423.15 338.86 405.89 348.83 423.15 358.8 423.15 338.86"/>
</g> <g>
<line class="st1" x1="410.11" y1="846.67" x2="472.64" y2="846.67"/>
<polygon class="st32" points="469.73 856.64 487 846.67 469.73 836.7 469.73 856.64"/>
</g> <g>
<line class="st1" x1="481.29" y1="865.12" x2="418.76" y2="865.12"/>
<polygon class="st32" points="421.67 855.15 404.4 865.12 421.67 875.09 421.67 855.15"/>
</g> <g>
<line class="st12" x1="193.97" y1="374.54" x2="221.5" y2="374.54"/>
<polygon class="st40" points="218.59 384.51 235.86 374.54 218.59 364.57 218.59 384.51"/>
</g> <g>
<line class="st12" x1="239.74" y1="365.38" x2="371.18" y2="365.38"/>
<polygon class="st40" points="368.27 375.35 385.54 365.38 368.27 355.41 368.27 375.35"/>
</g> <g>
<line class="st12" x1="278.3" y1="409.25" x2="371.18" y2="409.25"/>
<polygon class="st40" points="368.27 419.22 385.54 409.25 368.27 399.28 368.27 419.22"/>
</g> <g>
<polyline class="st12" points="25.81 279.64 25.81 324.98 60.75 322.98"/>
<polygon class="st40" points="57.84 332.95 75.11 322.98 57.84 313.01 57.84 332.95"/>
</g> <g>
<polyline class="st12" points="95.03 192.2 47.69 192.2 47.69 212.59"/>
<polygon class="st40" points="37.72 209.68 47.69 226.95 57.66 209.68 37.72 209.68"/>
</g> <g>
<line class="st12" x1="249.01" y1="392.99" x2="202.62" y2="392.99"/>
<polygon class="st40" points="205.53 383.02 188.26 392.99 205.53 402.96 205.53 383.02"/>
</g> <g>
<line class="st1" x1="410.11" y1="1015.82" x2="472.64" y2="1015.82"/>
<polygon class="st32" points="469.73 1025.79 487 1015.82 469.73 1005.85 469.73 1025.79"/>
</g> <g>
<line class="st1" x1="481.29" y1="1034.27" x2="418.76" y2="1034.27"/>
<polygon class="st32" points="421.67 1024.3 404.4 1034.27 421.67 1044.24 421.67 1024.3"/>
</g> <g>
<line class="st1" x1="410.11" y1="1078.42" x2="472.64" y2="1078.42"/>
<polygon class="st32" points="469.73 1088.39 487 1078.42 469.73 1068.45 469.73 1088.39"/>
</g> <g>
<line class="st1" x1="481.29" y1="1096.87" x2="418.76" y2="1096.87"/>
<polygon class="st32" points="421.67 1086.9 404.4 1096.87 421.67 1106.84 421.67 1086.9"/>
</g> <g>
<line class="st1" x1="383.22" y1="29.94" x2="445.75" y2="29.94"/>
<polygon class="st32" points="442.84 39.91 460.11 29.94 442.84 19.97 442.84 39.91"/>
</g> <g>
<line class="st1" x1="445.56" y1="743.15" x2="463.18" y2="743.15"/>
<polygon class="st32" points="448.48 733.18 431.21 743.15 448.48 753.12 448.48 733.18"/>
</g> <path class="st0" d="M423.88,678.7"/>
<path class="st0" d="M391.98,678.7"/> <g>
<line class="st1" x1="392.9" y1="856.22" x2="337.37" y2="856.22"/>
<polygon class="st32" points="340.28 846.24 323.01 856.22 340.28 866.19 340.28 846.24"/>
</g> <g> <g>
<rect class="st41" x="39.29" y="973.9" width="276.02" height="41.92" rx="5.8" ry="5.8"/>
<rect class="st8" x="39.29" y="973.9" width="276.02" height="41.92" rx="5.8" ry="5.8"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(84.14 999.4)"><tspan x="0" y="0">orbi\_plot\_raw\_data(y
= ratio)</tspan></text> </g> </g> </g>
<path class="st29" d="M52.62,984.88c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h0ZM73.76,989.39c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g> <g>
<rect class="st41" x="12.24" y="1030.4" width="302.37" height="41.92" rx="6.07" ry="6.07"/>
<rect class="st7" x="12.24" y="1030.4" width="302.37" height="41.92" rx="6.07" ry="6.07"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(57.09 1055.89)"><tspan x="0" y="0">orbi\_plot\_raw\_data(y
= tic \* it.ms)</tspan></text> </g> </g> </g>
<path class="st29" d="M25.57,1041.38c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM46.71,1045.88c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g>
<line class="st1" x1="342.54" y1="1053.82" x2="400.07" y2="1053.82"/>
<polygon class="st32" points="345.46 1043.85 328.19 1053.82 345.46 1063.79 345.46 1043.85"/>
</g> <g>
<line class="st1" x1="342.54" y1="1114.27" x2="400.07" y2="1114.27"/>
<polygon class="st32" points="345.46 1104.3 328.19 1114.27 345.46 1124.24 345.46 1104.3"/>
</g> <g>
<line class="st1" x1="342.54" y1="995.62" x2="400.07" y2="995.62"/>
<polygon class="st32" points="345.46 985.65 328.19 995.62 345.46 1005.59 345.46 985.65"/>
</g> <g>
<polyline class="st1" points="252.39 1248.63 30.87 1249.04 30.87 1115.7 74.25 1115.48"/>
<polygon class="st32" points="249.6 1258.64 266.75 1248.45 249.35 1238.69 249.6 1258.64"/>
</g> <g> <g>
<rect class="st41" x="573.89" y="563" width="241.2" height="41.92" rx="5.11" ry="5.11"/>
<rect class="st16" x="573.89" y="563" width="241.2" height="41.92" rx="5.11" ry="5.11"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(618.75 588.49)"><tspan x="0" y="0">orbi\_plot\_satellite\_peaks</tspan></text>
</g> </g> </g>
<path class="st29" d="M584.19,573.79c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM605.33,578.29c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g>
<line class="st1" x1="522.77" y1="583.39" x2="547.3" y2="583.39"/>
<polygon class="st32" points="544.39 593.37 561.66 583.39 544.39 573.42 544.39 593.37"/>
</g> <g> <g>
<rect class="st41" x="496.01" y="509.41" width="293.33" height="41.92" rx="5.65" ry="5.65"/>
<rect class="st9" x="496.01" y="509.41" width="293.33" height="41.92" rx="5.65" ry="5.65"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(540.86 534.91)"><tspan x="0" y="0">orbi\_plot\_isotopocule\_coverage</tspan></text>
</g> </g> </g>
<path class="st29" d="M506.3,520.2c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM527.44,524.7c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g>
<line class="st1" x1="405.71" y1="526.37" x2="466.62" y2="526.37"/>
<polygon class="st32" points="463.71 536.34 480.98 526.37 463.71 516.4 463.71 536.34"/>
</g> <g>
<line class="st1" x1="405.71" y1="483.3" x2="466.62" y2="483.3"/>
<polygon class="st32" points="463.71 493.27 480.98 483.3 463.71 473.33 463.71 493.27"/>
</g> <g>
<line class="st1" x1="405.71" y1="290.06" x2="466.62" y2="290.06"/>
<polygon class="st32" points="463.71 300.03 480.98 290.06 463.71 280.09 463.71 300.03"/>
</g> <g>
<line class="st1" x1="698.91" y1="709.85" x2="698.91" y2="751.64"/>
<polygon class="st32" points="688.94 748.72 698.91 765.99 708.88 748.72 688.94 748.72"/>
</g> <g>
<line class="st1" x1="117.92" y1="1139.83" x2="117.92" y2="1152.34"/>
<polygon class="st32" points="107.95 1149.43 117.92 1166.7 127.9 1149.43 107.95 1149.43"/>
</g> <g>
<polyline class="st1" points="614.35 54.2 614.35 88.43 592.93 88.43"/>
<polygon class="st32" points="595.84 78.46 578.57 88.43 595.84 98.4 595.84 78.46"/>
</g> <g>
<polyline class="st1" points="671.67 739.76 685.1 739.76 685.1 709.85"/>
<polygon class="st32" points="674.59 729.79 657.32 739.76 674.59 749.73 674.59 729.79"/>
</g> <g>
<line class="st1" x1="469.73" y1="198.7" x2="585.4" y2="198.7"/>
<polygon class="st32" points="582.48 208.68 599.75 198.7 582.48 188.73 582.48 208.68"/>
</g>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_find_isox.html">
<g> <g>
<rect class="st41" x="2.55" y="233.62" width="167.14" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st3" x="2.55" y="233.62" width="167.14" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M16.87,255.58l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(47.4 260.11)"><tspan x="0" y="0">orbi\_find\_isox</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_simplify_isox.html">
<g> <g>
<rect class="st41" x="4.83" y="362.81" width="180.36" height="41.92" rx="4.41" ry="4.41"/>
<rect class="st20" x="4.83" y="362.81" width="180.36" height="41.92" rx="4.41" ry="4.41"/>
</g>
<path class="st29" d="M16.14,383.77l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(40.68 388.3)"><tspan x="0" y="0">orbi\_simplify\_isox</tspan></text>
</g> </g> </g> </g> </a> <g>
<polyline class="st1" points="708.88 676.39 827.78 676.39 827.32 529.51 807.62 529.51"/>
<polygon class="st32" points="810.54 519.54 793.27 529.51 810.54 539.48 810.54 519.54"/>
</g>
<polyline class="st1" points="770 852.79 828.21 852.79 827.75 675.39"/>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_flag_outliers.html">
<g> <g>
<rect class="st41" x="494.35" y="999.76" width="187.36" height="41.92" rx="4.41" ry="4.41"/>
<rect class="st26" x="494.35" y="999.76" width="187.36" height="41.92" rx="4.41" ry="4.41"/>
</g>
<path class="st29" d="M505.67,1020.71l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(530.13 1026.25)"><tspan x="0" y="0">orbi\_flag\_outliers</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_segment_blocks.html">
<g> <g>
<rect class="st41" x="494.04" y="1066.7" width="202.76" height="41.92" rx="4.59" ry="4.59"/>
<rect class="st5" x="494.04" y="1066.7" width="202.76" height="41.92" rx="4.59" ry="4.59"/>
</g>
<path class="st29" d="M503.36,1087.65l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(527.82 1092.19)"><tspan x="0" y="0">orbi\_segment\_blocks</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_summarize_results.html">
<g> <g>
<rect class="st41" x="281.38" y="1153.82" width="233.24" height="41.92" rx="4.92" ry="4.92"/>
<rect class="st13" x="281.38" y="1153.82" width="233.24" height="41.92" rx="4.92" ry="4.92"/>
</g>
<path class="st29" d="M295.7,1174.78l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(319.24 1179.32)"><tspan x="0" y="0">orbi\_summarize\_results</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_analyze_shot_noise.html">
<g> <g>
<rect class="st41" x="74.25" y="1093.31" width="241.67" height="41.92" rx="4.68" ry="4.68"/>
<rect class="st7" x="74.25" y="1093.31" width="241.67" height="41.92" rx="4.68" ry="4.68"/>
</g>
<path class="st29" d="M85.57,1114.27l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(116.11 1118.81)"><tspan x="0" y="0">orbi\_analyze\_shot\_noise</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_plot_shot_noise.html">
<g> <g>
<rect class="st41" x="40.73" y="1172.75" width="210.56" height="41.92" rx="4.68" ry="4.68"/>
<rect class="st8" x="40.73" y="1172.75" width="210.56" height="41.92" rx="4.68" ry="4.68"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(82.58 1198.24)"><tspan x="0" y="0">orbi\_plot\_shot\_noise</tspan></text>
</g> </g> </g>
<path class="st29" d="M54.13,1183.71c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM75.27,1188.22c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_export_data_to_excel.html">
<g> <g>
<rect class="st41" x="273.17" y="1227.71" width="253.12" height="41.92" rx="4.68" ry="4.68"/>
<rect class="st13" x="273.17" y="1227.71" width="253.12" height="41.92" rx="4.68" ry="4.68"/>
</g>
<path class="st29" d="M284.49,1248.67l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(315.03 1253.2)"><tspan x="0" y="0">orbi\_export\_data\_to\_excel</tspan></text>
</g> </g> </g> </g> </a> <g> <g>
<rect class="st42" x="275.17" y="1301.39" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
<rect class="st13" x="275.17" y="1301.39" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(315.59 1326.92)"><tspan x="0" y="0">output
spreadsheet (xlsx)</tspan></text> </g> </g> </g> <g> <g>
<line class="st19" x1="289.76" y1="1320.93" x2="304.61" y2="1320.93"/>
<line class="st19" x1="289.75" y1="1323.93" x2="304.6" y2="1323.93"/>
<line class="st19" x1="289.78" y1="1326.92" x2="304.62" y2="1326.92"/>
<line class="st19" x1="289.77" y1="1329.92" x2="304.61" y2="1329.92"/>
<line class="st19" x1="293.16" y1="1320.93" x2="293.16" y2="1333.61"/>
<line class="st19" x1="297.17" y1="1320.93" x2="297.17" y2="1333.61"/>
<line class="st19" x1="301.39" y1="1320.93" x2="301.39" y2="1333.61"/>
</g>
<path class="st29" d="M289.81,1332.04v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82h0ZM290.63,1309.04c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
<g class="st38"> <g class="st38">
<text class="st37" transform="translate(296.61 1331.62) rotate(-90)"><tspan x="0" y="0">xlsx</tspan></text>
</g> </g> </g> </g> <g> <g>
<rect class="st41" x="387.69" y="638.01" width="319.21" height="71.84" rx="7.53" ry="7.53"/>
<rect class="st13" x="387.69" y="638.01" width="319.21" height="71.84" rx="7.53" ry="7.53"/>
</g>
<path class="st29" d="M399.01,658.97l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<path class="st29" d="M399.01,692.81l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_define_blocks_for_dual_inlet.html">
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(422.55 693.89)"><tspan x="0" y="0">orbi\_define\_blocks\_for\_dual\_inlet</tspan></text>
</g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_define_block_for_flow_injection.html">
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(422.55 663.69)"><tspan x="0" y="0">orbi\_define\_block\_for\_flow\_injection</tspan></text>
</g> </g> </g> </a> </g>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_get_blocks_info.html">
<g> <g>
<rect class="st41" x="552.01" y="770.16" width="211.58" height="41.92" rx="4.36" ry="4.36"/>
<rect class="st25" x="552.01" y="770.16" width="211.58" height="41.92" rx="4.36" ry="4.36"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(596.86 795.66)"><tspan x="0" y="0">orbi\_get\_blocks\_info</tspan></text>
</g> </g> </g>
<path class="st29" d="M573.1,805.66c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM570.83,795.67h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM573.1,783.85c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
</g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_get_isotopocule_coverage.html">
<g> <g>
<rect class="st41" x="495.7" y="461.62" width="286.52" height="41.92" rx="5.08" ry="5.08"/>
<rect class="st28" x="495.7" y="461.62" width="286.52" height="41.92" rx="5.08" ry="5.08"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(540.55 487.11)"><tspan x="0" y="0">orbi\_get\_isotopocule\_coverage</tspan></text>
</g> </g> </g>
<path class="st29" d="M516.79,497.12c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM514.52,487.12h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM516.79,475.31c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
</g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_adjust_block.html">
<g> <g>
<rect class="st41" x="469.83" y="719.86" width="183.66" height="41.92" rx="4.36" ry="4.36"/>
<rect class="st24" x="469.83" y="719.86" width="183.66" height="41.92" rx="4.36" ry="4.36"/>
</g>
<path class="st29" d="M484.14,740.82l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(508.68 745.36)"><tspan x="0" y="0">orbi\_adjust\_block</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_define_basepeak.html">
<g> <g>
<rect class="st41" x="290.39" y="911.27" width="218.69" height="41.92" rx="4.76" ry="4.76"/>
<rect class="st13" x="290.39" y="911.27" width="218.69" height="41.92" rx="4.76" ry="4.76"/>
</g>
<path class="st29" d="M301.71,932.23l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(332.24 936.77)"><tspan x="0" y="0">orbi\_define\_basepeak</tspan></text>
</g> </g> </g> </g> </a> <g>
<line class="st15" x1="553.7" y1="931.71" x2="535.49" y2="931.71"/>
<polygon class="st32" points="539 919.74 518.27 931.71 539 943.68 539 919.74"/>
</g> <g>
<line class="st15" x1="557.7" y1="1176.36" x2="539.49" y2="1176.36"/>
<polygon class="st32" points="543 1164.39 522.27 1176.36 543 1188.33 543 1164.39"/>
</g> <g>
<line class="st15" x1="568.91" y1="1322.18" x2="550.7" y2="1322.18"/>
<polygon class="st32" points="554.21 1310.21 533.48 1322.18 554.21 1334.15 554.21 1310.21"/>
</g> <g>
<line class="st15" x1="397.73" y1="953.56" x2="397.73" y2="1128.56"/>
<polygon class="st32" points="385.76 1125.06 397.73 1145.78 409.7 1125.06 385.76 1125.06"/>
</g> <path class="st6" d="M396.98,774.51"/> <g>
<polygon class="st31" points="656.89 901.77 594.72 901.77 563.64 932.35 594.72 962.93 656.89 962.93 687.97 932.35 656.89 901.77"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(614.8 925.68)"><tspan x="0" y="0">base</tspan></text>
</g> </g> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(614.8 948.08)"><tspan x="0" y="0">peak</tspan></text>
</g> </g> </g>
<path class="st29" d="M588.3,922.1c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM585.73,929.79c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
</g> <g>
<polygon class="st31" points="658.89 1145.78 596.72 1145.78 565.63 1176.36 596.72 1206.95 658.89 1206.95 689.97 1176.36 658.89 1145.78"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(618.69 1169.7)"><tspan x="0" y="0">ratio</tspan></text>
</g> </g> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(606.87 1192.1)"><tspan x="0" y="0">method</tspan></text>
</g> </g> </g>
<path class="st29" d="M590.3,1166.12c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM587.73,1173.8c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
</g> <g>
<polygon class="st31" points="640.17 1298.6 600 1298.6 576.92 1322.18 600 1345.76 640.17 1345.76 663.25 1322.18 640.17 1298.6"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(617.65 1328.69)"><tspan x="0" y="0">file</tspan></text>
</g> </g> </g>
<path class="st29" d="M599.58,1311.93c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM597.02,1319.62c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
</g> <g> <g>
<rect class="st35" x="4.42" y="443.16" width="283.19" height="360.35" rx="16.31" ry="16.31"/>
<rect class="st6" x="4.42" y="443.16" width="283.19" height="360.35" rx="16.31" ry="16.31"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st33" transform="translate(65.78 684.65)"><tspan x="0" y="0">information
functions</tspan></text> </g> </g> </g> <g class="st38">
<g class="st38"> <g class="st38">
<text class="st33" transform="translate(65.78 645.21)"><tspan x="0" y="0">processing
functions</tspan></text> </g> </g> </g> <g class="st38">
<g class="st38"> <g class="st38">
<text class="st33" transform="translate(64.58 523.14)"><tspan x="0" y="0">isoorbi
core functions</tspan></text> </g> </g> </g> <g class="st38">
<g class="st38"> <g class="st38">
<text class="st33" transform="translate(64.58 607.42)"><tspan x="0" y="0">obsolete
functions</tspan></text> </g> </g> </g> <g class="st38">
<g class="st38"> <g class="st38">
<text class="st33" transform="translate(64.58 552.9)"><tspan x="0" y="0">auxiliary
functions</tspan></text> </g> </g> <g class="st38"> <g class="st38">
<text class="st33" transform="translate(64.58 576.83)"><tspan x="0" y="0">(optional)</tspan></text>
</g> </g> </g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st33" transform="translate(65.78 724.83)"><tspan x="0" y="0">visualization
functions</tspan></text> </g> </g> </g> <g class="st38">
<g class="st38"> <g class="st38">
<text class="st33" transform="translate(65.78 759.61)"><tspan x="0" y="0">core
functions, essential</tspan></text> </g> </g> <g class="st38">
<g class="st38">
<text class="st33" transform="translate(65.78 783.54)"><tspan x="0" y="0">input
from user</tspan></text> </g> </g> </g>
<path class="st29" d="M25.92,639.89l1.23-12.12c.09-.85.67-1.49,1.38-1.49h7.29c.65,0,1.17.64,1.17,1.44,0,.17-.03.35-.07.5l-2,6.56h6.03c.87,0,1.59.87,1.59,1.95,0,.39-.1.78-.28,1.1l-8.31,14.93c-.26.46-.67.73-1.12.73h-.13c-.68,0-1.23-.68-1.23-1.51,0-.12.01-.24.04-.37l2.03-10.03h-6.23c-.77,0-1.38-.76-1.38-1.7h-.01,0Z"/>
<g>
<rect class="st41" x="19.68" y="502.19" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
<rect class="st13" x="19.68" y="502.19" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
</g> <g>
<rect class="st41" x="19.68" y="586.12" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
<rect class="st27" x="19.68" y="586.12" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
</g> <g>
<rect class="st41" x="19.68" y="543.92" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
<rect class="st18" x="19.68" y="543.92" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
</g> <g>
<polygon class="st30" points="36.66 748.63 35.41 748.63 17.5 766.25 35.41 783.88 36.66 783.88 54.58 766.25 36.66 748.63"/>
<path class="st29" d="M34.78,760.35c0-.82.66-1.48,1.48-1.48s1.48.66,1.48,1.48-.66,1.48-1.48,1.48-1.48-.66-1.48-1.48ZM33.3,764.78c0-.54.44-.98.98-.98h1.97c.54,0,.98.44.98.98v6.89h.98c.54,0,.98.44.98.98s-.44.98-.98.98h-3.94c-.54,0-.98-.44-.98-.98s.44-.98.98-.98h.98v-5.91h-.98c-.54,0-.98-.44-.98-.98h0Z"/>
</g>
<path class="st29" d="M34.22,693.87c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM31.95,683.87h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM34.22,672.06c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
<path class="st29" d="M24.24,709.53c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01,0ZM45.38,714.03c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st33" transform="translate(65.71 480.94)"><tspan x="0" y="0">data
files</tspan></text> </g> </g> </g> <g>
<rect class="st42" x="19.36" y="459.36" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
<rect class="st14" x="19.36" y="459.36" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
</g> </g> <g>
<rect class="st42" x="74.92" y="170.1" width="207.49" height="41.92" rx="4.73" ry="4.73"/>
<rect class="st21" x="74.92" y="170.1" width="207.49" height="41.92" rx="4.73" ry="4.73"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(108.45 196.89)"><tspan x="0" y="0">isotopocule
files (isox)</tspan></text> </g> </g> </g>
<path class="st29" d="M86.88,200.92c-.42,0-.76-.34-.76-.76v-18.21c0-.42.34-.76.76-.76h7.59v3.79c0,.84.68,1.52,1.52,1.52h3.79v13.66c0,.42-.34.76-.76.76h-12.14ZM86.88,178.92c-1.67,0-3.04,1.36-3.04,3.04v18.21c0,1.67,1.36,3.04,3.04,3.04h12.14c1.67,0,3.04-1.36,3.04-3.04v-13.92c0-.81-.32-1.58-.89-2.15l-4.3-4.29c-.57-.57-1.34-.89-2.14-.89h-7.85ZM89.54,191.06c-.63,0-1.14.51-1.14,1.14s.51,1.14,1.14,1.14h6.83c.63,0,1.14-.51,1.14-1.14s-.51-1.14-1.14-1.14h-6.83ZM89.54,195.61c-.63,0-1.14.51-1.14,1.14s.51,1.14,1.14,1.14h6.83c.63,0,1.14-.51,1.14-1.14s-.51-1.14-1.14-1.14h-6.83Z"/>
<g> <g>
<rect class="st42" x="231.41" y="4.61" width="148.63" height="41.92" rx="4.45" ry="4.45"/>
<rect class="st14" x="231.41" y="4.61" width="148.63" height="41.92" rx="4.45" ry="4.45"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(267.17 31.1)"><tspan x="0" y="0">raw
data files</tspan></text> </g> </g> </g>
<path class="st29" d="M254.66,36.72c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM237.51,15.28c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43V15.28h0Z"/>
<g class="st38"> <g class="st38">
<text class="st39" transform="translate(247.8 35.42) rotate(-90)"><tspan x="0" y="0">RAW</tspan></text>
</g> </g> </g> <g> <g>
<rect class="st42" x="248.8" y="59.14" width="114.46" height="48.33" rx="3.86" ry="3.86"/>
<rect class="st4" x="248.8" y="59.14" width="114.46" height="48.33" rx="3.86" ry="3.86"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(288.65 77.79)"><tspan x="0" y="0">peak
list</tspan></text> </g> <g class="st38">
<text class="st34" transform="translate(288.65 98.19)"><tspan x="0" y="0">(tsv/xlsx)</tspan></text>
</g> </g> </g>
<path class="st29" d="M257.19,84.56c-.42,0-.76-.34-.76-.76v-18.21c0-.42.34-.76.76-.76h7.59v3.79c0,.84.68,1.52,1.52,1.52h3.79v13.66c0,.42-.34.76-.76.76h-12.14,0ZM257.19,62.56c-1.67,0-3.04,1.36-3.04,3.04v18.21c0,1.67,1.36,3.04,3.04,3.04h12.14c1.67,0,3.04-1.36,3.04-3.04v-13.92c0-.81-.32-1.58-.89-2.15l-4.3-4.29c-.57-.57-1.34-.89-2.14-.89,0,0-7.85,0-7.85,0ZM259.85,74.7c-.63,0-1.14.51-1.14,1.14s.51,1.14,1.14,1.14h6.83c.63,0,1.14-.51,1.14-1.14s-.51-1.14-1.14-1.14h-6.83ZM259.85,79.25c-.63,0-1.14.51-1.14,1.14s.51,1.14,1.14,1.14h6.83c.63,0,1.14-.51,1.14-1.14s-.51-1.14-1.14-1.14h-6.83Z"/>
<rect class="st42" x="265.66" y="78.5" width="11.43" height="22.87" rx="3.86" ry="3.86"/>
<g> <g>
<line class="st19" x1="266.67" y1="89.69" x2="281.51" y2="89.69"/>
<line class="st19" x1="266.66" y1="92.69" x2="281.5" y2="92.69"/>
<line class="st19" x1="266.68" y1="95.68" x2="281.53" y2="95.68"/>
<line class="st19" x1="266.68" y1="98.68" x2="281.52" y2="98.68"/>
<line class="st19" x1="270.07" y1="89.69" x2="270.07" y2="102.37"/>
<line class="st19" x1="274.08" y1="89.69" x2="274.08" y2="102.37"/>
<line class="st19" x1="278.29" y1="89.69" x2="278.29" y2="102.37"/> </g>
<path class="st29" d="M266.71,100.8v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82ZM267.53,77.8c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5Z"/>
<g class="st38"> <g class="st38">
<text class="st37" transform="translate(273.52 100.38) rotate(-90)"><tspan x="0" y="0">xlsx</tspan></text>
</g> </g> </g> </g> <g>
<line class="st6" x1="342.86" y1="108.09" x2="342.86" y2="197.39"/>
<polygon class="st32" points="332.89 194.47 342.86 211.74 352.83 194.47 332.89 194.47"/>
</g> <g> <g>
<rect class="st36" x="4.83" y="94.66" width="235.73" height="41.92" rx="3.14" ry="3.14"/>
<rect class="st22" x="4.83" y="94.66" width="235.73" height="41.92" rx="3.14" ry="3.14"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(49.68 120.15)"><tspan x="0" y="0">IsoX
(external program)</tspan></text> </g> </g> </g>
<path class="st29" d="M19.98,102.24c-1.84,0-3.34,1.5-3.34,3.34v20.07c0,1.84,1.5,3.34,3.34,3.34h13.38c1.84,0,3.34-1.5,3.34-3.34v-20.07c0-1.84-1.5-3.34-3.34-3.34h-13.38ZM21.65,105.58h10.03c.92,0,1.67.75,1.67,1.67v1.67c0,.92-.75,1.67-1.67,1.67h-10.03c-.92,0-1.67-.75-1.67-1.67v-1.67c0-.92.75-1.67,1.67-1.67ZM23.32,113.94c0,.92-.75,1.67-1.67,1.67s-1.67-.75-1.67-1.67.75-1.67,1.67-1.67,1.67.75,1.67,1.67ZM21.65,120.63c-.92,0-1.67-.75-1.67-1.67s.75-1.67,1.67-1.67,1.67.75,1.67,1.67-.75,1.67-1.67,1.67ZM19.98,123.98c0-.92.75-1.67,1.67-1.67h5.02c.92,0,1.67.75,1.67,1.67s-.75,1.67-1.67,1.67h-5.02c-.92,0-1.67-.75-1.67-1.67ZM26.67,115.62c-.92,0-1.67-.75-1.67-1.67s.75-1.67,1.67-1.67,1.67.75,1.67,1.67-.75,1.67-1.67,1.67ZM28.34,118.96c0,.92-.75,1.67-1.67,1.67s-1.67-.75-1.67-1.67.75-1.67,1.67-1.67,1.67.75,1.67,1.67ZM31.68,115.62c-.92,0-1.67-.75-1.67-1.67s.75-1.67,1.67-1.67,1.67.75,1.67,1.67-.75,1.67-1.67,1.67ZM33.36,118.96c0,.92-.75,1.67-1.67,1.67s-1.67-.75-1.67-1.67.75-1.67,1.67-1.67,1.67.75,1.67,1.67ZM31.68,125.65c-.92,0-1.67-.75-1.67-1.67s.75-1.67,1.67-1.67,1.67.75,1.67,1.67-.75,1.67-1.67,1.67Z"/>
</g>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_read_raw.html">
<g> <g>
<rect class="st41" x="400.86" y="66.63" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st13" x="400.86" y="66.63" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M415.18,87.59l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(445.71 92.13)"><tspan x="0" y="0">orbi\_read\_raw</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_aggregate_raw.html">
<g> <g>
<rect class="st41" x="366.09" y="141.4" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st13" x="366.09" y="141.4" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M380.41,162.36l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(410.94 166.9)"><tspan x="0" y="0">orbi\_aggregate\_raw</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.html">
<g> <g>
<rect class="st41" x="325.07" y="217.79" width="249.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st13" x="325.07" y="217.79" width="249.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M339.39,238.75l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(369.92 243.29)"><tspan x="0" y="0">orbi\_identify\_isotopocules</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_plot_spectra.html">
<g> <g>
<rect class="st41" x="495.19" y="269.1" width="185.33" height="41.92" rx="5.65" ry="5.65"/>
<rect class="st9" x="495.19" y="269.1" width="185.33" height="41.92" rx="5.65" ry="5.65"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(540.04 294.59)"><tspan x="0" y="0">orbi\_plot\_spectra</tspan></text>
</g> </g> </g>
<path class="st29" d="M505.48,279.89c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM526.62,284.39c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_find_raw.html">
<g> <g>
<rect class="st41" x="470.68" y="12.28" width="167.14" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st2" x="470.68" y="12.28" width="167.14" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M485,34.24l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(515.53 38.77)"><tspan x="0" y="0">orbi\_find\_raw</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.html">
<g> <g>
<rect class="st41" x="495.17" y="316.98" width="228.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st9" x="495.17" y="316.98" width="228.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M509.49,337.94l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(533.82 342.48)"><tspan x="0" y="0">orbi\_filter\_isotopocules</tspan></text>
</g> </g> </g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_get_data.html">
<g> <g>
<rect class="st41" x="606.25" y="177.74" width="160.52" height="41.92" rx="5.08" ry="5.08"/>
<rect class="st28" x="606.25" y="177.74" width="160.52" height="41.92" rx="5.08" ry="5.08"/>
</g> <g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(651.1 203.23)"><tspan x="0" y="0">orbi\_get\_data</tspan></text>
</g> </g> </g>
<path class="st29" d="M627.34,213.24c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM625.07,203.24h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM627.34,191.43c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
</g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_calculate_ions.html">
<g> <g>
<rect class="st41" x="495.93" y="412.77" width="201.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st9" x="495.93" y="412.77" width="201.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st29" d="M510.25,433.73l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03,0Z"/>
<g class="st38"> <g class="st38"> <g class="st38">
<text class="st34" transform="translate(535.78 438.27)"><tspan x="0" y="0">orbi\_calculate\_ions</tspan></text>
</g> </g> </g> </g> </a>
</svg>

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/isoverse/isoorbi/issues).

For questions and other discussion, please use the [isoorbi slack
workspace](https://isoorbi.slack.com).

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" height="138.5"/></a>

This package is part of the isoverse suite of data tools for stable
isotopes. If you like the functionality that isoverse packages provide
to the geochemical community, please help us spread the word and include
an isoverse or individual package logo on one of your posters or slides.
All logos are posted in high resolution in [this
repository](https://github.com/isoverse/logos). If you have suggestions
for new features or other constructive feedback, please let us know on
this short [feeback form](https://www.isoverse.org/feedback/).
