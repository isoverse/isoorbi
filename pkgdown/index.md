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
It expects <code>.isox</code> files created by IsoX as input.

## Installation

You can install the current CRAN version of `isoorbi` with:

    install.packages("isoorbi")

To use the latest updates, you can install the development version of
`isoorbi` from [GitHub](https://github.com/) with:

    if(!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
    pak::pak("isoverse/isoorbi")

> Important: reading .raw files directly is made possible by the [rawrr
> package](https://github.com/fgcz/rawrr), which wraps Thermo’s
> [RawFileReader](https://github.com/thermofisherlsms/RawFileReader).
> The first time you read a .raw file, you will be asked to agree to
> [Thermo’s license
> agreement](https://github.com/fgcz/rawrr/blob/devel/inst/rawrrassembly/RawFileReaderLicense.txt)
> to proceed.

## Show me some code

    library(isoorbi)

    system.file(package = "isoorbi", "extdata", "testfile_flow.isox") |>
      orbi_read_isox() |>
      orbi_flag_satellite_peaks() |>
      orbi_define_basepeak(basepeak_def = "M0")|> 
      orbi_summarize_results(ratio_method = "sum") |>
      orbi_export_data_to_excel(file = "data_summary.xlsx")

## Package structure

<p>
Click on the individual functions to jump straight to their
documenation.
</p>
<?xml version="1.0" encoding="UTF-8"?>
<svg id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" viewBox="0 0 834.54 1399.61">
<metadata><c2pa:manifest xmlns:c2pa="http://c2pa.org/manifest">AAA0RWp1bWIAAAAeanVtZGMycGEAEQAQgAAAqgA4m3EDYzJwYQAAADQfanVtYgAAAEdqdW1kYzJtYQARABCAAACqADibcQN1cm46dXVpZDo3NTUwNGEyMy0zOTc3LTQyMGUtYWI5MC1lMDRlZWViOGY5MjAAAAABtGp1bWIAAAApanVtZGMyYXMAEQAQgAAAqgA4m3EDYzJwYS5hc3NlcnRpb25zAAAAANdqdW1iAAAAJmp1bWRjYm9yABEAEIAAAKoAOJtxA2MycGEuYWN0aW9ucwAAAACpY2JvcqFnYWN0aW9uc4GjZmFjdGlvbmtjMnBhLmVkaXRlZG1zb2Z0d2FyZUFnZW50bUFkb2JlIEZpcmVmbHlxZGlnaXRhbFNvdXJjZVR5cGV4U2h0dHA6Ly9jdi5pcHRjLm9yZy9uZXdzY29kZXMvZGlnaXRhbHNvdXJjZXR5cGUvY29tcG9zaXRlV2l0aFRyYWluZWRBbGdvcml0aG1pY01lZGlhAAAArGp1bWIAAAAoanVtZGNib3IAEQAQgAAAqgA4m3EDYzJwYS5oYXNoLmRhdGEAAAAAfGNib3KlamV4Y2x1c2lvbnOBomVzdGFydBjxZmxlbmd0aBlFtGRuYW1lbmp1bWJmIG1hbmlmZXN0Y2FsZ2ZzaGEyNTZkaGFzaFggFK3tOmw7oU9xgSpmakeEi87Q9G9je0JLT9B465JgXApjcGFkSQAAAAAAAAAAAAAAAgxqdW1iAAAAJGp1bWRjMmNsABEAEIAAAKoAOJtxA2MycGEuY2xhaW0AAAAB4GNib3KoaGRjOnRpdGxlb0dlbmVyYXRlZCBJbWFnZWlkYzpmb3JtYXRtaW1hZ2Uvc3ZnK3htbGppbnN0YW5jZUlEeCx4bXA6aWlkOmRhZmQ2ZjkyLWMzYTQtNDQ1Ni1hMWM1LTEyNGQ5MmIzY2VkOG9jbGFpbV9nZW5lcmF0b3J4N0Fkb2JlX0lsbHVzdHJhdG9yLzI5LjcgYWRvYmVfYzJwYS8wLjEyLjIgYzJwYS1ycy8wLjMyLjV0Y2xhaW1fZ2VuZXJhdG9yX2luZm+Bv2RuYW1lcUFkb2JlIElsbHVzdHJhdG9yZ3ZlcnNpb25kMjkuN/9pc2lnbmF0dXJleBlzZWxmI2p1bWJmPWMycGEuc2lnbmF0dXJlamFzc2VydGlvbnOComN1cmx4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AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAD2WQEAjpoIj3UqcQ/Z0gX0U88J5aQ4pS6Joqs4l3yjhMNydkhRyy3pC+bKRP1keOb3hnKE7auN0Du1GPg6qujk3zaG3to57ERPgyRQDKgVXbPDpYceYK6evx0sm1WS6ktz6p7tCmwo2e+ISkyEFk6XD1lCSeG9e/LcZMx+KwhDCp+2ffzedy+a1Rx2/x/XMuBCoW+AOAScf6T8BMVj/NOPYgwPkS0LNXxEudHS1REnIBiiRo8yNLtSgieGsIenRNUQy6JQsgsjgiRXzTMj4xuVhM+OFGyxmlJh7g2z1szioS0NjysDrGvS5eAiEMgeZnEr4rpVwBHT97BiTOZTHttqAsnnaA==</c2pa:manifest></metadata>
<!-- Generator: Adobe Illustrator 29.7.1, SVG Export Plug-In . SVG Version: 2.1.1 Build 8)  -->
<defs>
<style>
      .st0, .st1, .st2, .st3, .st4, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st12, .st13, .st14, .st15, .st16, .st17, .st18, .st19, .st20, .st21, .st22, .st23, .st24, .st25, .st26, .st27, .st28, .st29 {
        fill: none;
      }

      .st30 {
        fill: #010101;
      }

      .st1, .st2, .st31, .st32, .st3, .st4, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st12, .st13, .st14, .st15, .st16, .st17, .st18, .st19, .st20, .st21, .st22, .st23, .st24, .st25, .st26, .st27, .st28, .st29 {
        stroke-miterlimit: 10;
      }

      .st1, .st2, .st31, .st32, .st4, .st5, .st6, .st7, .st8, .st9, .st10, .st11, .st12, .st14, .st15, .st16, .st17, .st19, .st25, .st26, .st27, .st29 {
        stroke: #231f20;
      }

      .st1, .st5, .st6, .st7, .st8, .st9, .st11, .st12, .st13, .st18, .st19, .st27, .st29 {
        stroke-width: 2px;
      }

      .st1, .st13, .st25 {
        stroke-dasharray: 6 6;
      }

      .st2, .st3 {
        stroke-width: 2.36px;
      }

      .st2, .st3, .st10 {
        stroke-dasharray: 6;
      }

      .st33, .st34, .st35 {
        fill: #231f20;
      }

      .st36 {
        fill: #e2e2e2;
      }

      .st37 {
        fill: #fbb042;
      }

      .st31 {
        stroke-width: 1.53px;
      }

      .st31, .st32 {
        fill: #cdacd1;
      }

      .st32, .st23 {
        stroke-width: 2.66px;
      }

      .st3, .st13, .st18, .st21, .st22, .st23, .st24, .st28 {
        stroke: #878787;
      }

      .st4 {
        stroke-width: 2.15px;
      }

      .st5 {
        stroke-dasharray: 6.17 6.17;
      }

      .st34 {
        font-size: 19.94px;
      }

      .st34, .st38, .st35 {
        isolation: isolate;
      }

      .st34, .st35 {
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

      .st10, .st21, .st25, .st26 {
        stroke-width: 2.46px;
      }

      .st11 {
        stroke-dasharray: 7.95;
      }

      .st12 {
        stroke-dasharray: 7.75;
      }

      .st14, .st28 {
        stroke-width: 3px;
      }

      .st15 {
        stroke-width: 2.49px;
      }

      .st16, .st24 {
        stroke-width: 4px;
      }

      .st17 {
        stroke-dasharray: 7.26;
        stroke-width: 2.1px;
      }

      .st19 {
        stroke-dasharray: 6.98;
      }

      .st39 {
        font-size: 8px;
      }

      .st39, .st40 {
        font-family: Arial-BoldMT, Arial;
        font-weight: 700;
      }

      .st41 {
        fill: #878787;
      }

      .st20 {
        stroke: #000;
        stroke-width: .2px;
      }

      .st21, .st27 {
        stroke-dasharray: 5.93 5.93;
      }

      .st35 {
        font-size: 17px;
      }

      .st42 {
        fill: #5ebb55;
      }

      .st22 {
        stroke-width: 2.64px;
      }

      .st40 {
        font-size: 9px;
      }

      .st43 {
        fill: #5f88c6;
      }

      .st26 {
        stroke-dasharray: 5.95 5.95;
      }

      .st29 {
        stroke-dasharray: 7.04 7.04;
      }
    </style>
</defs> <g>
<polyline class="st18" points="288.92 107.86 288.92 124.37 261.62 124.37"/>
<polygon class="st41" points="264.54 114.4 247.27 124.37 264.54 134.34 264.54 114.4"/>
</g> <g>
<polyline class="st24" points="258.99 346.25 258.99 449.28 372.64 449.28"/>
<polygon class="st41" points="369.13 461.25 389.86 449.28 369.13 437.31 369.13 461.25"/>
</g> <g>
<line class="st24" x1="195.26" y1="213.63" x2="195.26" y2="279.03"/>
<polygon class="st41" points="183.29 275.52 195.26 296.25 207.23 275.52 183.29 275.52"/>
</g> <g>
<line class="st24" x1="126.51" y1="126.04" x2="126.51" y2="148.87"/>
<polygon class="st41" points="114.54 145.36 126.51 166.09 138.48 145.36 114.54 145.36"/>
</g> <g>
<line class="st24" x1="231.95" y1="46.56" x2="200.41" y2="72.65"/>
<polygon class="st41" points="195.34 61.25 187.28 83.8 210.83 79.5 195.34 61.25"/>
</g> <g> <line class="st16" x1="359.41" y1="26.66" x2="385.75" y2="53"/>
<polygon class="st33" points="374.8 58.98 397.93 65.18 391.73 42.05 374.8 58.98"/>
</g> <g> <g>
<rect class="st42" x="116.63" y="304.04" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st28" x="116.63" y="304.04" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M130.95,325l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(161.48 329.54)"><tspan x="0" y="0">orbi\_read\_isox</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="495.09" y="440.61" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st10" x="495.09" y="440.61" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M509.41,461.57l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(539.94 466.1)"><tspan x="0" y="0">orbi\_filter\_isox</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="8.7" y="887.18" width="302.37" height="41.92" rx="5.6" ry="5.6"/>
<rect class="st11" x="8.7" y="887.18" width="302.37" height="41.92" rx="5.6" ry="5.6"/>
</g> <g class="st38">
<text class="st35" transform="translate(53.55 912.68)"><tspan x="0" y="0">orbi\_plot\_raw\_data(y
= intensity)</tspan></text> </g>
<path class="st30" d="M22.03,898.16c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM43.17,902.66c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g> <g>
<rect class="st42" x="296.55" y="613.39" width="221.67" height="41.92" rx="5.03" ry="5.03"/>
<rect class="st14" x="296.55" y="613.39" width="221.67" height="41.92" rx="5.03" ry="5.03"/>
</g>
<path class="st30" d="M303.46,634.85l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(324.79 639.39)"><tspan x="0" y="0">orbi\_flag\_satellite\_peaks</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="495.49" y="882.22" width="272" height="41.92" rx="5.31" ry="5.31"/>
<rect class="st12" x="495.49" y="882.22" width="272" height="41.92" rx="5.31" ry="5.31"/>
</g>
<path class="st30" d="M506.8,903.18l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(533.34 907.72)"><tspan x="0" y="0">orbi\_flag\_weak\_isotopocules</tspan></text>
</g> </g> <g>
<line class="st16" x1="398.56" y1="409.63" x2="398.56" y2="590.62"/>
<polygon class="st33" points="386.6 587.12 398.56 607.84 410.53 587.12 386.6 587.12"/>
</g> <g>
<line class="st16" x1="398.71" y1="806.05" x2="398.71" y2="939.09"/>
<polygon class="st33" points="386.75 935.58 398.71 956.31 410.68 935.58 386.75 935.58"/>
</g> <g>
<line class="st16" x1="399.73" y1="1240.87" x2="399.73" y2="1255.95"/>
<polygon class="st33" points="387.76 1252.45 399.73 1273.17 411.7 1252.45 387.76 1252.45"/>
</g> <g>
<line class="st16" x1="399.73" y1="1311.94" x2="399.73" y2="1330.03"/>
<polygon class="st33" points="387.76 1326.53 399.73 1347.25 411.7 1326.53 387.76 1326.53"/>
</g>
<polyline class="st16" points="374.15 655.31 374.15 807.9 401.45 807.9 424.28 807.9 424.28 760.24"/>
<g> <line class="st16" x1="424.54" y1="655.31" x2="424.54" y2="666.86"/>
<polygon class="st33" points="412.58 663.36 424.54 684.09 436.51 663.36 412.58 663.36"/>
</g> <g>
<line class="st16" x1="449.72" y1="103.96" x2="449.72" y2="119.51"/>
<polygon class="st33" points="437.76 116.01 449.72 136.74 461.69 116.01 437.76 116.01"/>
</g> <g>
<line class="st16" x1="449.72" y1="184.12" x2="449.72" y2="199.67"/>
<polygon class="st33" points="437.76 196.17 449.72 216.89 461.69 196.17 437.76 196.17"/>
</g> <g>
<line class="st16" x1="450.29" y1="264.95" x2="450.29" y2="280.5"/>
<polygon class="st33" points="438.33 277 450.29 297.73 462.26 277 438.33 277"/>
</g> <g>
<line class="st16" x1="398.73" y1="346.96" x2="398.73" y2="362.51"/>
<polygon class="st33" points="386.77 359.01 398.73 379.74 410.7 359.01 386.77 359.01"/>
</g> <g>
<line class="st1" x1="410.11" y1="454.45" x2="472.64" y2="454.45"/>
<polygon class="st33" points="469.73 464.42 487 454.45 469.73 444.48 469.73 464.42"/>
</g> <g>
<line class="st1" x1="481.29" y1="472.9" x2="418.76" y2="472.9"/>
<polygon class="st33" points="421.67 462.93 404.4 472.9 421.67 482.87 421.67 462.93"/>
</g> <g>
<line class="st1" x1="410.11" y1="897.06" x2="472.64" y2="897.06"/>
<polygon class="st33" points="469.73 907.03 487 897.06 469.73 887.09 469.73 907.03"/>
</g> <g>
<line class="st1" x1="481.29" y1="915.51" x2="418.76" y2="915.51"/>
<polygon class="st33" points="421.67 905.54 404.4 915.51 421.67 925.48 421.67 905.54"/>
</g> <g>
<line class="st13" x1="200.97" y1="384.08" x2="239.5" y2="384.08"/>
<polygon class="st41" points="236.59 394.05 253.86 384.08 236.59 374.11 236.59 394.05"/>
</g> <g>
<polyline class="st13" points="287.61 326.1 311.07 326.1 311.07 361.01"/>
<polygon class="st41" points="301.1 358.1 311.07 375.37 321.04 358.1 301.1 358.1"/>
</g> <g>
<polyline class="st13" points="50.24 277 50.24 324.34 92.17 324.34"/>
<polygon class="st41" points="89.26 334.31 106.53 324.34 89.26 314.37 89.26 334.31"/>
</g> <g>
<polyline class="st13" points="95.03 192.59 47.69 192.59 47.69 220.98"/>
<polygon class="st41" points="37.72 218.07 47.69 235.34 57.66 218.07 37.72 218.07"/>
</g> <g>
<line class="st13" x1="248.15" y1="402.53" x2="209.62" y2="402.53"/>
<polygon class="st41" points="212.53 392.56 195.26 402.53 212.53 412.5 212.53 392.56"/>
</g> <g>
<line class="st1" x1="410.11" y1="1066.21" x2="472.64" y2="1066.21"/>
<polygon class="st33" points="469.73 1076.18 487 1066.21 469.73 1056.24 469.73 1076.18"/>
</g> <g>
<line class="st1" x1="481.29" y1="1084.66" x2="418.76" y2="1084.66"/>
<polygon class="st33" points="421.67 1074.69 404.4 1084.66 421.67 1094.63 421.67 1074.69"/>
</g> <g>
<line class="st1" x1="410.11" y1="1128.81" x2="472.64" y2="1128.81"/>
<polygon class="st33" points="469.73 1138.78 487 1128.81 469.73 1118.84 469.73 1138.78"/>
</g> <g>
<line class="st1" x1="481.29" y1="1147.26" x2="418.76" y2="1147.26"/>
<polygon class="st33" points="421.67 1137.29 404.4 1147.26 421.67 1157.23 421.67 1137.29"/>
</g> <g>
<line class="st1" x1="383.22" y1="30.33" x2="445.75" y2="30.33"/>
<polygon class="st33" points="442.84 40.3 460.11 30.33 442.84 20.36 442.84 40.3"/>
</g> <g>
<line class="st1" x1="445.56" y1="793.54" x2="463.18" y2="793.54"/>
<polygon class="st33" points="448.48 783.57 431.21 793.54 448.48 803.51 448.48 783.57"/>
</g> <path class="st0" d="M423.88,729.09"/>
<path class="st0" d="M391.98,729.09"/> <g>
<line class="st1" x1="392.9" y1="906.61" x2="337.37" y2="906.61"/>
<polygon class="st33" points="340.28 896.63 323.01 906.61 340.28 916.58 340.28 896.63"/>
</g> <g> <g>
<rect class="st42" x="39.29" y="1024.29" width="276.02" height="41.92" rx="5.8" ry="5.8"/>
<rect class="st8" x="39.29" y="1024.29" width="276.02" height="41.92" rx="5.8" ry="5.8"/>
</g> <g class="st38">
<text class="st35" transform="translate(84.14 1049.79)"><tspan x="0" y="0">orbi\_plot\_raw\_data(y
= ratio)</tspan></text> </g>
<path class="st30" d="M52.62,1035.27c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM73.76,1039.78c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g> <g>
<rect class="st42" x="12.24" y="1080.79" width="302.37" height="41.92" rx="6.07" ry="6.07"/>
<rect class="st7" x="12.24" y="1080.79" width="302.37" height="41.92" rx="6.07" ry="6.07"/>
</g> <g class="st38">
<text class="st35" transform="translate(57.09 1106.28)"><tspan x="0" y="0">orbi\_plot\_raw\_data(y
= tic \* it.ms)</tspan></text> </g>
<path class="st30" d="M25.57,1091.77c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM46.71,1096.27c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g>
<line class="st1" x1="342.54" y1="1104.21" x2="400.07" y2="1104.21"/>
<polygon class="st33" points="345.46 1094.24 328.19 1104.21 345.46 1114.18 345.46 1094.24"/>
</g> <g>
<line class="st1" x1="342.54" y1="1164.66" x2="400.07" y2="1164.66"/>
<polygon class="st33" points="345.46 1154.69 328.19 1164.66 345.46 1174.63 345.46 1154.69"/>
</g> <g>
<line class="st1" x1="342.54" y1="1046.01" x2="400.07" y2="1046.01"/>
<polygon class="st33" points="345.46 1036.04 328.19 1046.01 345.46 1055.98 345.46 1036.04"/>
</g> <g>
<polyline class="st1" points="252.39 1299.02 30.87 1299.43 30.87 1128.52"/>
<polygon class="st33" points="249.6 1309.03 266.75 1298.84 249.35 1289.08 249.6 1309.03"/>
</g> <g> <g>
<rect class="st42" x="573.89" y="613.39" width="241.2" height="41.92" rx="5.11" ry="5.11"/>
<rect class="st17" x="573.89" y="613.39" width="241.2" height="41.92" rx="5.11" ry="5.11"/>
</g> <g class="st38">
<text class="st35" transform="translate(618.75 638.88)"><tspan x="0" y="0">orbi\_plot\_satellite\_peaks</tspan></text>
</g>
<path class="st30" d="M584.19,624.18c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM605.33,628.68c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g>
<line class="st1" x1="522.77" y1="633.78" x2="547.3" y2="633.78"/>
<polygon class="st33" points="544.39 643.76 561.66 633.78 544.39 623.81 544.39 643.76"/>
</g> <g> <g>
<rect class="st42" x="496.01" y="548.8" width="293.33" height="41.92" rx="5.65" ry="5.65"/>
<rect class="st9" x="496.01" y="548.8" width="293.33" height="41.92" rx="5.65" ry="5.65"/>
</g> <g class="st38">
<text class="st35" transform="translate(540.86 574.3)"><tspan x="0" y="0">orbi\_plot\_isotopocule\_coverage</tspan></text>
</g>
<path class="st30" d="M506.3,559.59c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM527.44,564.09c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g>
<line class="st1" x1="405.71" y1="565.76" x2="466.62" y2="565.76"/>
<polygon class="st33" points="463.71 575.73 480.98 565.76 463.71 555.79 463.71 575.73"/>
</g> <g>
<line class="st1" x1="405.71" y1="519.69" x2="466.62" y2="519.69"/>
<polygon class="st33" points="463.71 529.66 480.98 519.69 463.71 509.72 463.71 529.66"/>
</g> <g>
<line class="st1" x1="698.91" y1="760.24" x2="698.91" y2="802.03"/>
<polygon class="st33" points="688.94 799.11 698.91 816.38 708.88 799.11 688.94 799.11"/>
</g> <g>
<line class="st1" x1="117.92" y1="1190.22" x2="117.92" y2="1202.73"/>
<polygon class="st33" points="107.95 1199.82 117.92 1217.09 127.9 1199.82 107.95 1199.82"/>
</g> <g>
<polyline class="st1" points="614.35 54.59 614.35 88.82 592.93 88.82"/>
<polygon class="st33" points="595.84 78.85 578.57 88.82 595.84 98.79 595.84 78.85"/>
</g> <g>
<polyline class="st1" points="671.67 790.15 685.1 790.15 685.1 760.24"/>
<polygon class="st33" points="674.59 780.18 657.32 790.15 674.59 800.12 674.59 780.18"/>
</g> <g>
<line class="st1" x1="469.73" y1="278.96" x2="585.4" y2="278.96"/>
<polygon class="st33" points="582.48 288.94 599.75 278.96 582.48 268.99 582.48 288.94"/>
</g> <g>
<line class="st1" x1="469.73" y1="199.09" x2="585.4" y2="199.09"/>
<polygon class="st33" points="582.48 209.07 599.75 199.09 582.48 189.12 582.48 209.07"/>
</g>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_find_isox.html">
<g> <g>
<rect class="st42" x="4.83" y="240.86" width="167.14" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st3" x="4.83" y="240.86" width="167.14" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M19.15,262.82l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(49.68 267.35)"><tspan x="0" y="0">orbi\_find\_isox</tspan></text>
</g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_simplify_isox.html">
<g> <g>
<rect class="st42" x="3.83" y="372.35" width="187.36" height="41.92" rx="4.41" ry="4.41"/>
<rect class="st21" x="3.83" y="372.35" width="187.36" height="41.92" rx="4.41" ry="4.41"/>
</g>
<path class="st30" d="M15.14,393.31l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(45.68 397.84)"><tspan x="0" y="0">orbi\_simplify\_isox</tspan></text>
</g> </g> </a> <g>
<polyline class="st1" points="767.49 841.71 827.78 841.71 827.32 572.9 807.62 572.9"/>
<polygon class="st33" points="810.54 562.93 793.27 572.9 810.54 582.87 810.54 562.93"/>
</g> <g> <g>
<rect class="st42" x="494.35" y="1050.15" width="187.36" height="41.92" rx="4.41" ry="4.41"/>
<rect class="st27" x="494.35" y="1050.15" width="187.36" height="41.92" rx="4.41" ry="4.41"/>
</g>
<path class="st30" d="M505.67,1071.1l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(530.13 1076.64)"><tspan x="0" y="0">orbi\_flag\_outliers</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="494.04" y="1117.09" width="202.76" height="41.92" rx="4.59" ry="4.59"/>
<rect class="st5" x="494.04" y="1117.09" width="202.76" height="41.92" rx="4.59" ry="4.59"/>
</g>
<path class="st30" d="M503.36,1138.04l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(527.82 1142.58)"><tspan x="0" y="0">orbi\_segment\_blocks</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="281.38" y="1204.21" width="233.24" height="41.92" rx="4.92" ry="4.92"/>
<rect class="st14" x="281.38" y="1204.21" width="233.24" height="41.92" rx="4.92" ry="4.92"/>
</g>
<path class="st30" d="M295.7,1225.17l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(319.24 1229.71)"><tspan x="0" y="0">orbi\_summarize\_results</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="74.25" y="1143.7" width="241.67" height="41.92" rx="4.68" ry="4.68"/>
<rect class="st7" x="74.25" y="1143.7" width="241.67" height="41.92" rx="4.68" ry="4.68"/>
</g>
<path class="st30" d="M85.57,1164.66l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(116.11 1169.2)"><tspan x="0" y="0">orbi\_analyze\_shot\_noise</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="40.73" y="1223.14" width="210.56" height="41.92" rx="4.68" ry="4.68"/>
<rect class="st8" x="40.73" y="1223.14" width="210.56" height="41.92" rx="4.68" ry="4.68"/>
</g> <g class="st38">
<text class="st35" transform="translate(82.58 1248.63)"><tspan x="0" y="0">orbi\_plot\_shot\_noise</tspan></text>
</g>
<path class="st30" d="M54.13,1234.1c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM75.27,1238.61c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g> <g> <g>
<rect class="st42" x="273.17" y="1278.1" width="253.12" height="41.92" rx="4.68" ry="4.68"/>
<rect class="st14" x="273.17" y="1278.1" width="253.12" height="41.92" rx="4.68" ry="4.68"/>
</g>
<path class="st30" d="M284.49,1299.06l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(315.03 1303.59)"><tspan x="0" y="0">orbi\_export\_data\_to\_excel</tspan></text>
</g> </g> <g> <g>
<rect class="st43" x="275.17" y="1351.78" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
<rect class="st14" x="275.17" y="1351.78" width="248.29" height="41.92" rx="4.73" ry="4.73"/>
</g> <g class="st38">
<text class="st35" transform="translate(315.59 1377.31)"><tspan x="0" y="0">output
spreadsheet (xlsx)</tspan></text> </g> <g> <g>
<line class="st20" x1="289.76" y1="1371.32" x2="304.61" y2="1371.32"/>
<line class="st20" x1="289.75" y1="1374.32" x2="304.6" y2="1374.32"/>
<line class="st20" x1="289.78" y1="1377.31" x2="304.62" y2="1377.31"/>
<line class="st20" x1="289.77" y1="1380.31" x2="304.61" y2="1380.31"/>
<line class="st20" x1="293.16" y1="1371.32" x2="293.16" y2="1384"/>
<line class="st20" x1="297.17" y1="1371.32" x2="297.17" y2="1384"/>
<line class="st20" x1="301.39" y1="1371.32" x2="301.39" y2="1384"/> </g>
<path class="st30" d="M289.81,1382.43v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82ZM290.63,1359.43c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5,0Z"/>
<text class="st40" transform="translate(296.61 1382.01) rotate(-90)"><tspan x="0" y="0">xlsx</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="387.69" y="688.4" width="319.21" height="71.84" rx="7.53" ry="7.53"/>
<rect class="st14" x="387.69" y="688.4" width="319.21" height="71.84" rx="7.53" ry="7.53"/>
</g>
<path class="st30" d="M399.01,709.36l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<path class="st30" d="M399.01,743.2l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(422.55 744.28)"><tspan x="0" y="0">orbi\_define\_blocks\_for\_dual\_inlet</tspan></text>
</g> <g class="st38">
<text class="st35" transform="translate(422.55 714.08)"><tspan x="0" y="0">orbi\_define\_block\_for\_flow\_injection</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="552.01" y="820.55" width="211.58" height="41.92" rx="4.36" ry="4.36"/>
<rect class="st26" x="552.01" y="820.55" width="211.58" height="41.92" rx="4.36" ry="4.36"/>
</g> <g class="st38">
<text class="st35" transform="translate(596.86 846.05)"><tspan x="0" y="0">orbi\_get\_blocks\_info</tspan></text>
</g>
<path class="st30" d="M573.1,856.05c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM570.83,846.06h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM573.1,834.24c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
</g> <g> <g>
<rect class="st42" x="495.7" y="494.01" width="286.52" height="41.92" rx="5.08" ry="5.08"/>
<rect class="st29" x="495.7" y="494.01" width="286.52" height="41.92" rx="5.08" ry="5.08"/>
</g> <g class="st38">
<text class="st35" transform="translate(540.55 519.5)"><tspan x="0" y="0">orbi\_get\_isotopocule\_coverage</tspan></text>
</g>
<path class="st30" d="M516.79,529.51c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM514.52,519.51h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM516.79,507.7c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
</g> <g> <g>
<rect class="st42" x="469.83" y="770.25" width="183.66" height="41.92" rx="4.36" ry="4.36"/>
<rect class="st25" x="469.83" y="770.25" width="183.66" height="41.92" rx="4.36" ry="4.36"/>
</g>
<path class="st30" d="M484.14,791.21l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(508.68 795.75)"><tspan x="0" y="0">orbi\_adjust\_block</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="290.39" y="961.66" width="218.69" height="41.92" rx="4.76" ry="4.76"/>
<rect class="st14" x="290.39" y="961.66" width="218.69" height="41.92" rx="4.76" ry="4.76"/>
</g>
<path class="st30" d="M301.71,982.62l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(332.24 987.16)"><tspan x="0" y="0">orbi\_define\_basepeak</tspan></text>
</g> </g> <g>
<line class="st16" x1="553.7" y1="982.1" x2="535.49" y2="982.1"/>
<polygon class="st33" points="539 970.13 518.27 982.1 539 994.07 539 970.13"/>
</g> <g>
<line class="st16" x1="557.7" y1="1226.75" x2="539.49" y2="1226.75"/>
<polygon class="st33" points="543 1214.78 522.27 1226.75 543 1238.72 543 1214.78"/>
</g> <g>
<line class="st16" x1="568.91" y1="1372.57" x2="550.7" y2="1372.57"/>
<polygon class="st33" points="554.21 1360.6 533.48 1372.57 554.21 1384.54 554.21 1360.6"/>
</g> <g>
<line class="st16" x1="541.08" y1="405.01" x2="522.87" y2="405.01"/>
<polygon class="st33" points="526.38 393.04 505.65 405.01 526.38 416.98 526.38 393.04"/>
</g> <g>
<line class="st16" x1="397.73" y1="1003.95" x2="397.73" y2="1178.95"/>
<polygon class="st33" points="385.76 1175.45 397.73 1196.17 409.7 1175.45 385.76 1175.45"/>
</g> <path class="st6" d="M396.98,824.9"/> <g>
<polygon class="st32" points="656.89 952.16 594.72 952.16 563.64 982.74 594.72 1013.32 656.89 1013.32 687.97 982.74 656.89 952.16"/>
<g class="st38">
<text class="st35" transform="translate(614.8 976.07)"><tspan x="0" y="0">base</tspan></text>
<text class="st35" transform="translate(614.8 998.47)"><tspan x="0" y="0">peak</tspan></text>
</g>
<path class="st30" d="M588.3,972.49c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM585.73,980.18c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
</g> <g>
<polygon class="st32" points="658.89 1196.17 596.72 1196.17 565.63 1226.75 596.72 1257.34 658.89 1257.34 689.97 1226.75 658.89 1196.17"/>
<g class="st38">
<text class="st35" transform="translate(618.69 1220.09)"><tspan x="0" y="0">ratio</tspan></text>
<text class="st35" transform="translate(606.87 1242.49)"><tspan x="0" y="0">method</tspan></text>
</g>
<path class="st30" d="M590.3,1216.51c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM587.73,1224.19c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
</g> <g>
<polygon class="st32" points="640.17 1348.99 600 1348.99 576.92 1372.57 600 1396.15 640.17 1396.15 663.25 1372.57 640.17 1348.99"/>
<g class="st38">
<text class="st35" transform="translate(617.65 1379.08)"><tspan x="0" y="0">file</tspan></text>
</g>
<path class="st30" d="M599.58,1362.32c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM597.02,1370.01c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
</g> <g>
<polygon class="st32" points="633 381.43 571.8 381.43 548.72 405.01 571.8 428.59 633 428.59 656.08 405.01 633 381.43"/>
<g class="st38">
<text class="st35" transform="translate(582.21 411.52)"><tspan x="0" y="0">method</tspan></text>
</g>
<path class="st30" d="M571.38,394.76c0-1.41,1.15-2.56,2.56-2.56s2.56,1.15,2.56,2.56-1.15,2.56-2.56,2.56-2.56-1.15-2.56-2.56ZM568.82,402.45c0-.94.76-1.71,1.71-1.71h3.42c.94,0,1.71.76,1.71,1.71v11.96h1.71c.94,0,1.71.76,1.71,1.71s-.76,1.71-1.71,1.71h-6.83c-.94,0-1.71-.76-1.71-1.71s.76-1.71,1.71-1.71h1.71v-10.25h-1.71c-.94,0-1.71-.76-1.71-1.71h-.01Z"/>
</g> <g> <g>
<rect class="st36" x="4.42" y="493.55" width="283.19" height="360.35" rx="16.31" ry="16.31"/>
<rect class="st6" x="4.42" y="493.55" width="283.19" height="360.35" rx="16.31" ry="16.31"/>
</g> <g class="st38">
<text class="st34" transform="translate(65.78 735.04)"><tspan x="0" y="0">information
functions</tspan></text> </g> <g class="st38">
<text class="st34" transform="translate(65.78 695.6)"><tspan x="0" y="0">processing
functions</tspan></text> </g> <g class="st38">
<text class="st34" transform="translate(64.58 573.53)"><tspan x="0" y="0">isoorbi
core functions</tspan></text> </g> <g class="st38">
<text class="st34" transform="translate(64.58 657.81)"><tspan x="0" y="0">obsolete
functions</tspan></text> </g> <g class="st38">
<text class="st34" transform="translate(64.58 603.29)"><tspan x="0" y="0">auxiliary
functions</tspan></text>
<text class="st34" transform="translate(64.58 627.22)"><tspan x="0" y="0">(optional)</tspan></text>
</g> <g class="st38">
<text class="st34" transform="translate(65.78 775.22)"><tspan x="0" y="0">visualization
functions</tspan></text> </g> <g class="st38">
<text class="st34" transform="translate(65.78 810)"><tspan x="0" y="0">core
functions, essential</tspan></text>
<text class="st34" transform="translate(65.78 833.93)"><tspan x="0" y="0">input
from user</tspan></text> </g>
<path class="st30" d="M25.92,690.28l1.23-12.12c.09-.85.67-1.49,1.38-1.49h7.29c.65,0,1.17.64,1.17,1.44,0,.17-.03.35-.07.5l-2,6.56h6.03c.87,0,1.59.87,1.59,1.95,0,.39-.1.78-.28,1.1l-8.31,14.93c-.26.46-.67.73-1.12.73h-.13c-.68,0-1.23-.68-1.23-1.51,0-.12.01-.24.04-.37l2.03-10.03h-6.23c-.77,0-1.38-.76-1.38-1.7h-.01Z"/>
<g>
<rect class="st42" x="19.68" y="552.58" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
<rect class="st14" x="19.68" y="552.58" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
</g> <g>
<rect class="st42" x="19.68" y="636.51" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
<rect class="st28" x="19.68" y="636.51" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
</g> <g>
<rect class="st42" x="19.68" y="594.31" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
<rect class="st19" x="19.68" y="594.31" width="31.26" height="31.26" rx="5.69" ry="5.69"/>
</g> <g>
<polygon class="st31" points="36.66 799.02 35.41 799.02 17.5 816.64 35.41 834.27 36.66 834.27 54.58 816.64 36.66 799.02"/>
<path class="st30" d="M34.78,810.74c0-.82.66-1.48,1.48-1.48s1.48.66,1.48,1.48-.66,1.48-1.48,1.48-1.48-.66-1.48-1.48ZM33.3,815.17c0-.54.44-.98.98-.98h1.97c.54,0,.98.44.98.98v6.89h.98c.54,0,.98.44.98.98s-.44.98-.98.98h-3.94c-.54,0-.98-.44-.98-.98s.44-.98.98-.98h.98v-5.91h-.98c-.54,0-.98-.44-.98-.98h.01Z"/>
</g>
<path class="st30" d="M34.22,744.26c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM31.95,734.26h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM34.22,722.45c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
<path class="st30" d="M24.24,759.92c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM45.38,764.42c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
<g class="st38">
<text class="st34" transform="translate(65.71 531.33)"><tspan x="0" y="0">data
files</tspan></text> </g> <g>
<rect class="st43" x="19.36" y="509.75" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
<rect class="st15" x="19.36" y="509.75" width="32.52" height="32.52" rx="1.55" ry="1.55"/>
</g> </g> <g>
<rect class="st43" x="74.92" y="170.49" width="207.49" height="41.92" rx="4.73" ry="4.73"/>
<rect class="st22" x="74.92" y="170.49" width="207.49" height="41.92" rx="4.73" ry="4.73"/>
</g> <g class="st38">
<text class="st35" transform="translate(108.45 197.28)"><tspan x="0" y="0">isotopocule
files (isox)</tspan></text> </g>
<path class="st30" d="M86.88,201.31c-.42,0-.76-.34-.76-.76v-18.21c0-.42.34-.76.76-.76h7.59v3.79c0,.84.68,1.52,1.52,1.52h3.79v13.66c0,.42-.34.76-.76.76h-12.14,0ZM86.88,179.31c-1.67,0-3.04,1.36-3.04,3.04v18.21c0,1.67,1.36,3.04,3.04,3.04h12.14c1.67,0,3.04-1.36,3.04-3.04v-13.92c0-.81-.32-1.58-.89-2.15l-4.3-4.29c-.57-.57-1.34-.89-2.14-.89h-7.85ZM89.54,191.45c-.63,0-1.14.51-1.14,1.14s.51,1.14,1.14,1.14h6.83c.63,0,1.14-.51,1.14-1.14s-.51-1.14-1.14-1.14h-6.83ZM89.54,196c-.63,0-1.14.51-1.14,1.14s.51,1.14,1.14,1.14h6.83c.63,0,1.14-.51,1.14-1.14s-.51-1.14-1.14-1.14h-6.83Z"/>
<g> <g>
<rect class="st43" x="231.41" y="5" width="148.63" height="41.92" rx="4.45" ry="4.45"/>
<rect class="st15" x="231.41" y="5" width="148.63" height="41.92" rx="4.45" ry="4.45"/>
</g> <g class="st38">
<text class="st35" transform="translate(267.17 31.49)"><tspan x="0" y="0">raw
data files</tspan></text> </g>
<path class="st30" d="M254.66,37.11c.47,0,.86-.39.86-.86v-15.44h-4.29c-.95,0-1.72-.77-1.72-1.72v-4.29h-8.58c-.47,0-.86.39-.86.86v20.58c0,.47.39.86.86.86h13.73ZM237.51,15.67c0-1.89,1.54-3.43,3.43-3.43h8.87c.91,0,1.78.36,2.43,1l4.85,4.85c.64.64,1,1.52,1,2.43v15.73c0,1.89-1.54,3.43-3.43,3.43h-13.72c-1.89,0-3.43-1.54-3.43-3.43V15.67Z"/>
<text class="st39" transform="translate(247.8 35.81) rotate(-90)"><tspan x="0" y="0">RAW</tspan></text>
</g> <g> <g>
<rect class="st43" x="248.8" y="59.53" width="114.46" height="48.33" rx="3.86" ry="3.86"/>
<rect class="st4" x="248.8" y="59.53" width="114.46" height="48.33" rx="3.86" ry="3.86"/>
</g> <g class="st38">
<text class="st35" transform="translate(288.65 78.18)"><tspan x="0" y="0">peak
list</tspan><tspan x="0" y="20.4">(tsv/xlsx)</tspan></text> </g>
<path class="st30" d="M257.19,84.95c-.42,0-.76-.34-.76-.76v-18.21c0-.42.34-.76.76-.76h7.59v3.79c0,.84.68,1.52,1.52,1.52h3.79v13.66c0,.42-.34.76-.76.76h-12.14,0ZM257.19,62.95c-1.67,0-3.04,1.36-3.04,3.04v18.21c0,1.67,1.36,3.04,3.04,3.04h12.14c1.67,0,3.04-1.36,3.04-3.04v-13.92c0-.81-.32-1.58-.89-2.15l-4.3-4.29c-.57-.57-1.34-.89-2.14-.89h-7.85ZM259.85,75.09c-.63,0-1.14.51-1.14,1.14s.51,1.14,1.14,1.14h6.83c.63,0,1.14-.51,1.14-1.14s-.51-1.14-1.14-1.14h-6.83ZM259.85,79.64c-.63,0-1.14.51-1.14,1.14s.51,1.14,1.14,1.14h6.83c.63,0,1.14-.51,1.14-1.14s-.51-1.14-1.14-1.14h-6.83Z"/>
<rect class="st43" x="265.66" y="78.89" width="11.43" height="22.87" rx="3.86" ry="3.86" transform="translate(542.75 180.65) rotate(180)"/>
<g> <g>
<line class="st20" x1="266.67" y1="90.08" x2="281.51" y2="90.08"/>
<line class="st20" x1="266.66" y1="93.08" x2="281.5" y2="93.08"/>
<line class="st20" x1="266.68" y1="96.07" x2="281.53" y2="96.07"/>
<line class="st20" x1="266.68" y1="99.07" x2="281.52" y2="99.07"/>
<line class="st20" x1="270.07" y1="90.08" x2="270.07" y2="102.76"/>
<line class="st20" x1="274.08" y1="90.08" x2="274.08" y2="102.76"/>
<line class="st20" x1="278.29" y1="90.08" x2="278.29" y2="102.76"/> </g>
<path class="st30" d="M266.71,101.19v-19.72c0-.46.37-.82.82-.82h8.22v4.1c0,.91.74,1.65,1.65,1.65h4.1v14.79c0,.46-.37.82-.82.82h-13.15c-.46,0-.82-.37-.82-.82ZM267.53,78.19c-1.81,0-3.28,1.47-3.28,3.28v19.72c0,1.81,1.47,3.28,3.28,3.28h13.15c1.81,0,3.28-1.47,3.28-3.28v-15.07c0-.88-.34-1.71-.96-2.33l-4.65-4.65c-.61-.61-1.44-.96-2.32-.96h-8.5,0Z"/>
<text class="st40" transform="translate(273.52 100.77) rotate(-90)"><tspan x="0" y="0">xlsx</tspan></text>
</g> </g> <g>
<line class="st6" x1="342.86" y1="108.48" x2="342.86" y2="201.78"/>
<polygon class="st33" points="332.89 198.86 342.86 216.13 352.83 198.86 332.89 198.86"/>
</g> <g> <g>
<rect class="st37" x="4.83" y="95.05" width="235.73" height="41.92" rx="3.14" ry="3.14"/>
<rect class="st23" x="4.83" y="95.05" width="235.73" height="41.92" rx="3.14" ry="3.14"/>
</g> <g class="st38">
<text class="st35" transform="translate(49.68 120.54)"><tspan x="0" y="0">IsoX
(external program)</tspan></text> </g>
<path class="st30" d="M19.98,102.63c-1.84,0-3.34,1.5-3.34,3.34v20.07c0,1.84,1.5,3.34,3.34,3.34h13.38c1.84,0,3.34-1.5,3.34-3.34v-20.07c0-1.84-1.5-3.34-3.34-3.34h-13.38ZM21.65,105.97h10.03c.92,0,1.67.75,1.67,1.67v1.67c0,.92-.75,1.67-1.67,1.67h-10.03c-.92,0-1.67-.75-1.67-1.67v-1.67c0-.92.75-1.67,1.67-1.67ZM23.32,114.33c0,.92-.75,1.67-1.67,1.67s-1.67-.75-1.67-1.67.75-1.67,1.67-1.67,1.67.75,1.67,1.67ZM21.65,121.02c-.92,0-1.67-.75-1.67-1.67s.75-1.67,1.67-1.67,1.67.75,1.67,1.67-.75,1.67-1.67,1.67ZM19.98,124.37c0-.92.75-1.67,1.67-1.67h5.02c.92,0,1.67.75,1.67,1.67s-.75,1.67-1.67,1.67h-5.02c-.92,0-1.67-.75-1.67-1.67ZM26.67,116.01c-.92,0-1.67-.75-1.67-1.67s.75-1.67,1.67-1.67,1.67.75,1.67,1.67-.75,1.67-1.67,1.67ZM28.34,119.35c0,.92-.75,1.67-1.67,1.67s-1.67-.75-1.67-1.67.75-1.67,1.67-1.67,1.67.75,1.67,1.67ZM31.68,116.01c-.92,0-1.67-.75-1.67-1.67s.75-1.67,1.67-1.67,1.67.75,1.67,1.67-.75,1.67-1.67,1.67ZM33.36,119.35c0,.92-.75,1.67-1.67,1.67s-1.67-.75-1.67-1.67.75-1.67,1.67-1.67,1.67.75,1.67,1.67ZM31.68,126.04c-.92,0-1.67-.75-1.67-1.67s.75-1.67,1.67-1.67,1.67.75,1.67,1.67-.75,1.67-1.67,1.67Z"/>
</g>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_read_raw.html">
<g> <g>
<rect class="st42" x="400.86" y="67.02" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st14" x="400.86" y="67.02" width="172.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M415.18,87.98l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(445.71 92.52)"><tspan x="0" y="0">orbi\_read\_raw</tspan></text>
</g> </g> </a>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_aggregate_raw.html">
<g> <g>
<rect class="st42" x="366.09" y="141.79" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st14" x="366.09" y="141.79" width="207.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M380.41,162.75l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(410.94 167.29)"><tspan x="0" y="0">orbi\_aggregate\_raw</tspan></text>
</g> </g> </a> <g> <g>
<rect class="st42" x="325.07" y="222.18" width="249.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st14" x="325.07" y="222.18" width="249.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M339.39,243.14l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(369.92 247.68)"><tspan x="0" y="0">orbi\_identify\_isotopocules</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="606.25" y="259.54" width="185.33" height="41.92" rx="5.65" ry="5.65"/>
<rect class="st9" x="606.25" y="259.54" width="185.33" height="41.92" rx="5.65" ry="5.65"/>
</g> <g class="st38">
<text class="st35" transform="translate(651.1 285.04)"><tspan x="0" y="0">orbi\_plot\_spectra</tspan></text>
</g>
<path class="st30" d="M616.54,270.33c0-.92-.74-1.66-1.66-1.66s-1.66.74-1.66,1.66v17.47c0,2.3,1.86,4.16,4.16,4.16h20.8c.92,0,1.66-.74,1.66-1.66s-.74-1.66-1.66-1.66h-20.8c-.46,0-.83-.37-.83-.83v-17.47h-.01ZM637.68,274.83c.65-.65.65-1.71,0-2.36s-1.71-.65-2.36,0l-5.47,5.48-2.98-2.98c-.65-.65-1.71-.65-2.36,0l-5.82,5.82c-.65.65-.65,1.71,0,2.36s1.71.65,2.36,0l4.65-4.64,2.98,2.98c.65.65,1.71.65,2.36,0l6.65-6.65h-.01Z"/>
</g>
<a xlink:href="https://isoorbi.isoverse.org/reference/orbi_find_raw.html">
<g> <g>
<rect class="st42" x="470.68" y="12.67" width="167.14" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st2" x="470.68" y="12.67" width="167.14" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M485,34.63l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11,0-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(515.53 39.16)"><tspan x="0" y="0">orbi\_find\_raw</tspan></text>
</g> </g> </a> <g> <g>
<rect class="st42" x="356.82" y="304.45" width="218.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st14" x="356.82" y="304.45" width="218.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M371.14,325.41l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(401.67 329.95)"><tspan x="0" y="0">orbi\_get\_isotopocules</tspan></text>
</g> </g> <g> <g>
<rect class="st42" x="606.25" y="178.13" width="160.52" height="41.92" rx="5.08" ry="5.08"/>
<rect class="st29" x="606.25" y="178.13" width="160.52" height="41.92" rx="5.08" ry="5.08"/>
</g> <g class="st38">
<text class="st35" transform="translate(651.1 203.62)"><tspan x="0" y="0">orbi\_get\_data</tspan></text>
</g>
<path class="st30" d="M627.34,213.63c8.03,0,14.54-6.51,14.54-14.54s-6.51-14.54-14.54-14.54-14.54,6.51-14.54,14.54,6.51,14.54,14.54,14.54ZM625.07,203.63h1.36v-3.63h-1.36c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h2.73c.76,0,1.36.61,1.36,1.36v5h.45c.76,0,1.36.61,1.36,1.36s-.61,1.36-1.36,1.36h-4.54c-.76,0-1.36-.61-1.36-1.36s.61-1.36,1.36-1.36h0ZM627.34,191.82c1,0,1.82.81,1.82,1.82s-.81,1.82-1.82,1.82-1.82-.81-1.82-1.82.81-1.82,1.82-1.82Z"/>
</g> <g> <g>
<rect class="st42" x="298.06" y="383.86" width="201.29" height="41.92" rx="4.23" ry="4.23"/>
<rect class="st14" x="298.06" y="383.86" width="201.29" height="41.92" rx="4.23" ry="4.23"/>
</g>
<path class="st30" d="M312.38,404.82l1.12-10.98c.08-.77.61-1.35,1.25-1.35h6.61c.59,0,1.06.58,1.06,1.3,0,.15-.02.31-.07.46l-1.81,5.94h5.46c.79,0,1.44.79,1.44,1.77,0,.36-.09.7-.25,1l-7.53,13.53c-.23.41-.61.66-1.01.66h-.11c-.62,0-1.12-.62-1.12-1.37,0-.11.01-.22.04-.33l1.84-9.08h-5.64c-.69,0-1.25-.69-1.25-1.54h-.03Z"/>
<g class="st38">
<text class="st35" transform="translate(342.91 409.36)"><tspan x="0" y="0">orbi\_calculate\_ions</tspan></text>
</g> </g>
</svg>

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/isoverse/isoorbi/issues).

For questions and other discussion, please use the [isoorbi slack
workspace](https://isoorbi.slack.com).
