# Find isox files

Finds all .isox files in a folder.

## Usage

``` r
orbi_find_isox(folder, recursive = TRUE)
```

## Arguments

- folder:

  path to a folder with isox files

- recursive:

  whether to find files recursively

## Examples

``` r
# all .isox files provided with the isoorbi package
orbi_find_isox(system.file("extdata", package = "isoorbi"))
#> [1] "/home/runner/work/_temp/Library/isoorbi/extdata/testfile_dual_inlet.isox"
#> [2] "/home/runner/work/_temp/Library/isoorbi/extdata/testfile_flow.isox"      
#> [3] "/home/runner/work/_temp/Library/isoorbi/extdata/testfile_shotnoise.isox" 
```
