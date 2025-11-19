# Package options

These options are best set via `orbi_options()` and queried via
`orbi_get_option()`. However, the base functions
[`options()`](https://rdrr.io/r/base/options.html) and
[`getOption()`](https://rdrr.io/r/base/options.html) work as well but
require an `isoorbi.` prefix (the package name and a dot) for the option
name. Setting an option to a value of `NULL` means that the default is
used. `orbi_get_options()` is available as an additional convenience
function to retrieve a subset of options with a regular expression
pattern.

## Usage

``` r
orbi_options(...)

orbi_get_options(pattern = NULL)

orbi_get_option(x)
```

## Arguments

- ...:

  set package options, syntax identical to
  [`options()`](https://rdrr.io/r/base/options.html)

- pattern:

  to retrieve multiple options (as a list) with a shared pattern

- x:

  name of the specific option to retrieve

## Functions

- `orbi_options()`: set/get option values

- `orbi_get_options()`: get a subset of option values that fit a pattern

- `orbi_get_option()`: retrieve the current value of one option (option
  must be defined for the package)

## Options for the isoorbi package

- `di_ref_name`: the text label for dual inlet reference blocks

- `di_sample_name`: the text label for dual inlet sample blocks

- `data_type_data`: the text used to flag raw data as actually being
  data

- `data_type_startup`: the text used to flag raw data as being part of
  the startup

- `data_type_changeover`: the text used to flag raw data as being part
  of a changeover

- `data_type_unused`: the text used to flag raw data as being unused

- `aggregators`: data aggregators for pulling data out of raw files. The
  list of available aggregators is accessible via
  `orbi_get_option("aggregators")`. Individiual aggregators are
  available via the shortcut helper function
  `orbi_get_aggregator("standard")`. Register new/overwrite existing
  aggregators via
  [`orbi_register_aggregator()`](https://isoorbi.isoverse.org/reference/orbi_aggregator.md).

- `debug`: turn on debug mode

- `auto_use_ansi`: whether to automatically enable correct rendering of
  stylized (ansi) output in HTML reports from notebooks that call
  [`library(isoorbi)`](https://isoorbi.isoverse.org/). Can be turned off
  by calling `isoorbi::orbi_options(auto_use_ansi = FALSE)` **before**
  call [`library(isoorbi)`](https://isoorbi.isoverse.org/).

## Examples

``` r
# All default options
orbi_get_options()
#> $di_ref_name
#> [1] "ref"
#> 
#> $di_sample_name
#> [1] "sam"
#> 
#> $data_type_data
#> [1] "data"
#> 
#> $data_type_startup
#> [1] "startup"
#> 
#> $data_type_changeover
#> [1] "changeover"
#> 
#> $data_type_unused
#> [1] "unused"
#> 
#> $aggregators
#> $aggregators$minimal
#> ────────────────────────────── Aggregator minimal ──────────────────────────────
#> Dataset file_info:
#>  → filename = as.character(sub(FileName, pattern = ".raw", replacement = "",
#> fixed = TRUE))
#>  → creation_date = as.POSIXct(CreationDate)
#>  → in_aquisition = as.logical(InAquisition)
#> Dataset scans:
#>  → scan.no = as.integer(scan.no)
#>  → time.min = as.numeric(StartTime)
#>  → tic = as.numeric(TIC)
#>  → it.ms = as.numeric(`Ion Injection Time (ms)`)
#>  → resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
#>  → microscans = as.integer(`Micro Scan Count`)
#> Dataset peaks:
#>  → scan.no = as.integer(scan.no)
#>  → mzMeasured = as.numeric(mass)
#>  → intensity = as.numeric(intensity)
#>  → baseline = as.numeric(baseline)
#>  → peakNoise = as.numeric(noise)
#>  → peakResolution = as.numeric(resolution)
#>  → isRefPeak = as.logical(is_ref)
#>  → isLockPeak = as.logical(is_lock_peak)
#> Dataset spectra:
#>  → scan.no = as.integer(scan.no)
#>  → mz = as.numeric(mass)
#>  → intensity = as.numeric(intensity)
#> 
#> $aggregators$standard
#> ────────────────────────────── Aggregator standard ─────────────────────────────
#> Dataset file_info:
#>  → filename = as.character(sub(FileName, pattern = ".raw", replacement = "",
#> fixed = TRUE))
#>  → creation_date = as.POSIXct(CreationDate)
#>  → in_aquisition = as.logical(InAquisition)
#>  → (.*) = as.character(all_matches("(.*)"))
#> Dataset scans:
#>  → scan.no = as.integer(scan.no)
#>  → time.min = as.numeric(StartTime)
#>  → tic = as.numeric(TIC)
#>  → it.ms = as.numeric(`Ion Injection Time (ms)`)
#>  → resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
#>  → microscans = as.integer(`Micro Scan Count`)
#>  → basePeakMz = as.numeric(BasePeakMass)
#>  → basePeakIntensity = as.numeric(BasePeakIntensity)
#>  → lowMass = as.numeric(LowMass)
#>  → highMass = as.numeric(HighMass)
#>  → rawOvFtT = as.numeric(RawOvFtT)
#>  → intensCompFactor = as.numeric(`OT Intens Comp Factor`)
#>  → agc = as.character(AGC)
#>  → agcTarget = as.integer(`AGC Target`)
#>  → numberLockmassesFound = as.integer(`Number of LM Found`)
#>  → analyzerTemperature = as.numeric(`Analyzer Temperature`)
#> Dataset peaks:
#>  → scan.no = as.integer(scan.no)
#>  → mzMeasured = as.numeric(mass)
#>  → intensity = as.numeric(intensity)
#>  → baseline = as.numeric(baseline)
#>  → peakNoise = as.numeric(noise)
#>  → peakResolution = as.numeric(resolution)
#>  → isRefPeak = as.logical(is_ref)
#>  → isLockPeak = as.logical(is_lock_peak)
#> Dataset spectra:
#>  → scan.no = as.integer(scan.no)
#>  → mz = as.numeric(mass)
#>  → intensity = as.numeric(intensity)
#> 
#> $aggregators$extended
#> ────────────────────────────── Aggregator extended ─────────────────────────────
#> Dataset file_info:
#>  → filename = as.character(sub(FileName, pattern = ".raw", replacement = "",
#> fixed = TRUE))
#>  → creation_date = as.POSIXct(CreationDate)
#>  → in_aquisition = as.logical(InAquisition)
#>  → (.*) = as.character(all_matches("(.*)"))
#> Dataset scans:
#>  → scan.no = as.integer(scan.no)
#>  → time.min = as.numeric(StartTime)
#>  → tic = as.numeric(TIC)
#>  → it.ms = as.numeric(`Ion Injection Time (ms)`)
#>  → resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
#>  → microscans = as.integer(`Micro Scan Count`)
#>  → basePeakMz = as.numeric(BasePeakMass)
#>  → basePeakIntensity = as.numeric(BasePeakIntensity)
#>  → lowMass = as.numeric(LowMass)
#>  → highMass = as.numeric(HighMass)
#>  → rawOvFtT = as.numeric(RawOvFtT)
#>  → intensCompFactor = as.numeric(`OT Intens Comp Factor`)
#>  → agc = as.character(AGC)
#>  → agcTarget = as.integer(`AGC Target`)
#>  → numberLockmassesFound = as.integer(`Number of LM Found`)
#>  → analyzerTemperature = as.numeric(`Analyzer Temperature`)
#>  → (.*) = as.character(all_matches("(.*)"))
#> Dataset peaks:
#>  → scan.no = as.integer(scan.no)
#>  → mzMeasured = as.numeric(mass)
#>  → intensity = as.numeric(intensity)
#>  → baseline = as.numeric(baseline)
#>  → peakNoise = as.numeric(noise)
#>  → peakResolution = as.numeric(resolution)
#>  → isRefPeak = as.logical(is_ref)
#>  → isLockPeak = as.logical(is_lock_peak)
#> Dataset spectra:
#>  → scan.no = as.integer(scan.no)
#>  → mz = as.numeric(mass)
#>  → intensity = as.numeric(intensity)
#> 
#> 
#> $debug
#> [1] FALSE
#> 
#> $auto_use_ansi
#> [1] TRUE
#> 

# Options that contain 'data' in the name
orbi_get_options("data")
#> $data_type_data
#> [1] "data"
#> 
#> $data_type_startup
#> [1] "startup"
#> 
#> $data_type_changeover
#> [1] "changeover"
#> 
#> $data_type_unused
#> [1] "unused"
#> 

# Specific option
orbi_get_option("data_type_unused")
#> [1] "unused"

# Change an option
orbi_options(data_type_unused = "flagged")
orbi_get_option("data_type_unused")
#> [1] "flagged"

# Change back to default
orbi_options(data_type_unused = NULL)
orbi_get_option("data_type_unused")
#> [1] "unused"
```
