library(testthat)
library(dplyr, warn.conflicts = FALSE)

# run the tests
isoorbi::orbi_check_isoraw(accept_license = TRUE)
test_check("isoorbi")
