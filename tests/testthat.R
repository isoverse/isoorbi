library(testthat)
library(dplyr, warn.conflicts = FALSE)

# run the tests
if (!on_cran()) {
  # install isoraw but only if we're not on cran
  orbi_check_isoraw(accept_license = TRUE)
}
test_check("isoorbi")
