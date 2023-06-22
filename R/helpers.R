#' Create factor in order
#'
#' Helper function to create factors with levels in the order of data appearance. This is a simpler implementation of [forcats::fct_inorder()]
#' @param x vector or factor
#' @return factor with levels in order of appearance
#' @export
factor_in_order <- function(x) {
  if (!is.factor(x)) x <- as.factor(x)
  idx <- as.integer(x)[!duplicated(x)]
  idx <- idx[!is.na(idx)]
  return(factor(x, levels = levels(x)[idx]))
}
