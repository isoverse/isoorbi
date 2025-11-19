# Export data to excel

This functions exports the `dataset` into an Excel file. If the
`dataset` is aggregated data, use the `include` parameter to decide
which part of the data to export.

## Usage

``` r
orbi_export_data_to_excel(
  dataset,
  file,
  dbl_digits = 7,
  int_format = "0",
  dbl_format = sprintf(sprintf("%%.%sf", dbl_digits), 0),
  include = c("file_info", "summary"),
  show_progress = rlang::is_interactive()
)
```

## Arguments

- dataset:

  An aggregated dataset or a data frame of peaks (i.e. works directly
  after
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
  as well as with a tibble from [orbi_get_data(peaks =
  everything())](https://isoorbi.isoverse.org/reference/orbi_get_data.md)
  or when reading from an IsoX file)

- file:

  file path to export the file - recursively creates the directory if
  non-existent

- dbl_digits:

  how many digits to show for dbls (all are exported)

- int_format:

  the excel formatting style for integers

- dbl_format:

  the excel formatting style for doubles (created automatically from the
  dbl_digits parameter)

- include:

  which tibbles to include if `dataset` is aggregated data. By default
  includes just the file_info and summary (if it exists) to avoid
  accidentally generating massive excel files. To export e.g. the
  file_info, scans, and peaks, use
  `include = c("file_info", "scans", "peaks")` (using
  [`orbi_filter_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.md)
  first is highly recommended if exporting peaks). To export the entire
  spectra in the dataset, use `include = c("file_info", "spectra")`.

- show_progress:

  whether to show a progress bar, by default always enabled when running
  interactively e.g. inside RStudio (and disabled in a notebook), turn
  off with `show_progress = FALSE`

## Value

returns dataset invisibly for use in pipes
