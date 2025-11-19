# Calculate isotopocule ratio

This function calculates the ratio of two isotopocules (the `numerator`
and `denominator`). This function averages multiple measurements of each
using the `ratio_method` and returns a single value. Normally this
function is not called directly by the user, but via the function
[`orbi_summarize_results()`](https://isoorbi.isoverse.org/reference/orbi_summarize_results.md),
which calculates isotopocule ratios and other results for an entire
dataset.

## Usage

``` r
orbi_calculate_summarized_ratio(
  numerator,
  denominator,
  ratio_method = c("direct", "mean", "sum", "median", "geometric_mean", "slope",
    "weighted_sum")
)
```

## Arguments

- numerator:

  Column(s) used as numerator; contains ion counts

- denominator:

  Column used as denominator; contains ion counts

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

## Value

Single value ratio between the isotopocules defined as `numerator` and
`denominator` calculated using the `ratio_method`.

## Examples

``` r
df <-
  system.file("extdata", "testfile_flow.isox", package = "isoorbi") |>
  orbi_read_isox()
#> ✔ [151ms] orbi_read_isox() loaded 6449 peaks for 1 compound (HSO4-) with 5
#> isotopocules (M0, 33S, 17O, 34S, and 18O) from testfile_flow.isox

ions_18O <- dplyr::filter(df, isotopocule == "18O")$ions.incremental
ions_M0 <- dplyr::filter(df, isotopocule == "M0")$ions.incremental

orbi_calculate_summarized_ratio(
  numerator = ions_18O, denominator = ions_M0, ratio_method = "sum"
)
#> [1] 0.01066211

orbi_calculate_summarized_ratio(
  numerator = ions_18O, denominator = ions_M0, ratio_method = "slope"
)
#> [1] 0.01071427
```
