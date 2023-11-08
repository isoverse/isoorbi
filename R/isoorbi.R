#' @keywords internal
"_PACKAGE"

# import functions/packages used so frequently we don't want to always refer to the namespace explicitly
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @importFrom utils data
#' @importFrom ggplot2 %+% scale_color_manual scale_fill_manual
NULL


# quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c("."))
