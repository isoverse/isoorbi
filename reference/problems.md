# Retrieve parsing problems

This function retrieves parsing problems encountered during the reading
and processing of files.

This function prints out parsing problems encountered during the reading
and processing of files.

## Usage

``` r
orbi_get_problems(obj, strip_ansi = TRUE)

orbi_show_problems(obj)
```

## Arguments

- obj:

  data object that holds problems information

- strip_ansi:

  whether to remove ansi characters from the message, yes by default

## Value

tibble data frame with a list of problems encountered during processing
