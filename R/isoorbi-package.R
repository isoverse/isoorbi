#' @details
#'
#' Resources:
#'   * Website for the isoorbi package: <https://isoorbi.isoverse.org>
#'   * Package options: [orbi_options]
"_PACKAGE"

## usethis namespace: start
#' @import cli
#' @import rlang
#' @import dplyr
#' @import tidyr
#' @importFrom stats setNames
#' @importFrom tibble is_tibble
#' @importFrom utils data
#' @importFrom ggplot2 %+% scale_color_manual scale_fill_manual
#' @importFrom knitr knit_print
## usethis namespace: end
NULL


# quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c("."))
