# Check for the isoorbi raw file reader

By default, this will install the isoraw reader if it is missing or
outdated, and will ask the user to agree to Thermo's license agreement
for the [Thermo
RawFileReader](https://github.com/thermofisherlsms/RawFileReader) before
proceeding. This function runs automatically during a raw file read and
does not usually need to be called directly by the user.

## Usage

``` r
orbi_check_isoraw(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "0.2.2",
  source = paste0("https://github.com/isoverse/isoorbi/releases/download/isoraw-v",
    min_version),
  accept_license = FALSE,
  ...
)
```

## Arguments

- install_if_missing:

  install the reader if it's missing

- reinstall_if_outdated:

  install the reader if it's outdated (i.e. not at least `min_version`)

- reinstall_always:

  whether to (re-)install no matter what

- min_version:

  the minimum version number required

- source:

  the URL (or local path) where to find the raw file reader, by default
  this is the latests release of the executables on github

- accept_license:

  explicitly accept Thermo's license agreement (if this is FALSE and the
  license has not previously been accepted, you will be asked about it)

- ...:

  passed on to `download.file` if (re-) installing the reader
