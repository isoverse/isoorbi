#' @keywords internal
"_PACKAGE"

# import functions/packages used so frequently we don't want to always refer to the namespace explicitly
#' @import dplyr
#' @import tidyr
#' @importFrom rlang !!! !!

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# quiets concerns of R CMD check about .data in tidyverse functions (to avoid other global variables)
#' @importFrom rlang .data

# quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c("."))
