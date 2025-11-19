# Find raw files

Finds all .raw files in a folder.

## Usage

``` r
orbi_find_raw(folder, pattern = NULL, include_cache = TRUE, recursive = TRUE)
```

## Arguments

- folder:

  path to a folder with raw files

- pattern:

  provide a name pattern to find only specific raw files

- include_cache:

  whether to include .raw.cache.zip folders in the absence of the
  corresponding .raw file so that copies of the cache are read even in
  the absence of the original raw files

- recursive:

  whether to find files recursively

## Examples

``` r
# all .raw files provided with the isoorbi package
orbi_find_raw(system.file("extdata", package = "isoorbi"))
#> [1] "/home/runner/work/_temp/Library/isoorbi/extdata/nitrate_test_10scans.raw"
#> [2] "/home/runner/work/_temp/Library/isoorbi/extdata/nitrate_test_1scan.raw"  
```
