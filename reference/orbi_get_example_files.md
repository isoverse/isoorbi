# Get available example file(s)

This function will provide the path(s) to example file(s). If a
requested file is not yet available locally but is available on
https://github.com/isoverse/isodata, it will download it from there into
local storage. By default, it will download only cache files
(.raw.cache.zip) instead of the original .raw files because the cache
files are significantely smaller. Todownload the original raw files
instead, use `download_raw_files = TRUE`.

## Usage

``` r
orbi_get_example_files(
  filenames,
  download_raw_files = FALSE,
  download_always = FALSE
)
```

## Arguments

- filenames:

  names of the example files

- download_raw_files:

  should the original raw files be downloaded? By default only cache
  files (raw.cache.zip) are downloaded as they are usually much smaller.
  However, they will not work for retrieving additional spectra. To
  download the original spectra, switch to `download_raw_files = TRUE`

- download_always:

  whether to download files anew even if they already exist locally

## Value

file path(s) that can be passed directly to
[`orbi_read_raw()`](https://isoorbi.isoverse.org/reference/orbi_read_raw.md)
