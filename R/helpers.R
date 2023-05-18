# helper function to create factors with levels in the order of data appearnace
# this is a simpler implementation of forcats::fct_inorder (no other need for forcats dependency)
factor_in_order <- function(x) {
  if (!is.factor(x)) x <- as.factor(x)
  idx <- as.integer(x)[!duplicated(x)]
  idx <- idx[!is.na(idx)]
  return(factor(x, levels = levels(x)[idx]))
}
