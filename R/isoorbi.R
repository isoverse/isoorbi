#' @details
#'
#' Resources:
#'   * Website for the isoorbi package: <https://isoorbi.isoverse.org>
#'   * Package options: [isoorbi_options]
"_PACKAGE"

## usethis namespace: start
#' @import cli
#' @import rlang
#' @import dplyr
#' @import tidyr
#' @importFrom tibble is_tibble
#' @importFrom utils data
#' @importFrom ggplot2 %+% scale_color_manual scale_fill_manual
## usethis namespace: end
NULL


# quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c("."))
