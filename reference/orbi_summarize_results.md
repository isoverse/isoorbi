# Generate the results table

Contains the logic to generate the results table. It passes the
`ratio_method` parameter to the
[`orbi_calculate_summarized_ratio()`](https://isoorbi.isoverse.org/reference/orbi_calculate_summarized_ratio.md)
function for ratio calculations.

## Usage

``` r
orbi_summarize_results(
  dataset,
  ratio_method = c("mean", "sum", "median", "geometric_mean", "slope", "weighted_sum"),
  .by = c("block", "sample_name", "segment", "data_group", "data_type", "injection"),
  include_flagged_data = FALSE,
  include_unused_data = FALSE
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

- ratio_method:

  Method for computing the ratio. **Please note well**: the formula used
  to calculate ion ratios matters! Do not simply use arithmetic mean.
  The best option may depend on the type of data you are processing
  (e.g., MS1 versus M+1 fragmentation). `ratio_method` can be one of the
  following:

  - `mean`: arithmetic mean of ratios from individual scans.

  - `sum`: sum of all ions of the numerator across all scans divided by
    the sum of all ions observed for the denominator across all scans.

  - `geometric_mean`: geometric mean of ratios from individual scans.

  - `slope`: The ratio is calculated using the slope obtained from a
    linear regression model that is weighted by the `numerator x`, using
    `stats::lm(x ~ y + 0, weights = x)`.

  - `weighted_sum`: A derivative of the `sum` option. The weighing
    function ensures that each scan contributes equal weight to the
    ratio calculation, i.e. scans with more ions in the Orbitrap do not
    contribute disproportionately to the total `sum` of `x` and `y` that
    is used to calculate `x/y`.

- .by:

  additional grouping columns for the results summary (akin to dplyr's
  `.by` parameter e.g. in
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)).
  If not set by the user, all columns in the parameter's default values
  are used, if present in the dataset. Note that the order of these is
  also used to arrange the summary.

- include_flagged_data:

  whether to include flagged data in the calculations (FALSE by default)

- include_unused_data:

  whether to include unused data in the calculations (FALSE by default),
  in addition to peaks actually flagged as
  orbi_get_option("data_type_data")

## Value

Returns a results summary table (as tibble if `dataset` is a tibble, as
`dataset$tibble` if `dataset` is aggregated raw data) with the columns
`filename`, `compound`, `isotopocule` and `basepeak` as well as the
grouping columns from the `.by` parameter that are part of the input
`dataset`. Additionally this function adds the following results
columns: `start_scan.no`, `end_scan.no`, `start_time.min`,
`mean_time.min`, `end_time.min`, `ratio`, `ratio_sem`,
`ratio_relative_sem_permil`, `shot_noise_permil`, `No.of.Scans`,
`minutes_to_1e6_ions`

- `ratio`: The isotope ratio between the `isotopocule` and the
  `basepeak`, calculated using the `ratio_method`

- `ratio_sem`: Standard error of the mean for the ratio

- `number_of_scans`: Number of scans used for the final ratio
  calculation

- `minutes_to_1e6_ions`: Time in minutes it would take to observe 1
  million ions of the `isotopocule` used as numerator of the ratio
  calculation.

- `shot_noise_permil`: Estimate of the shot noise (more correctly
  thermal noise) of the reported ratio in permil.

- `ratio_relative_sem_permil`: Relative standard error of the reported
  ratio in permil

## Examples

``` r
fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
df <- orbi_read_isox(file = fpath) |>
      orbi_simplify_isox() |>
      orbi_define_basepeak("M0")  |>
      orbi_summarize_results(ratio_method = "sum")
#> ✔ [21ms] orbi_read_isox() loaded 6449 peaks for 1 compound (HSO4-) with 5
#> isotopocules (M0, 33S, 17O, 34S, and 18O) from testfile_flow.isox
#> ✔ [5ms] orbi_simplify_isox() kept columns filepath, filename, scan.no,
#> time.min, compound, isotopocule, ions.incremental, tic, and it.ms
#> ✔ [146ms] orbi_define_basepeak() set M0 as the ratio denominator and calculated
#> 5.16k ratio values for 4 isotopocules (33S, 17O, 34S, and 18O)
#> ✔ [88ms] orbi_summarize_results() summarized ratios from 5.16k peak using the
#> sum method and grouping the data by filename, compound, basepeak, and
#> isotopocule
```
