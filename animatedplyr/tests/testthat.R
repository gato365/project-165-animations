library(testthat)
library(animatedplyr)

test_check("animatedplyr")


# Everything (the normal command):
devtools::test()

# A single layer:
testthat::test_file("tests/testthat/test-03-sampling-reproducibility.R")

# Filter by name across all files:
devtools::test(filter = "reproducib")     # matches test-03 by file stem

# Full check (what CRAN/CI runs):
devtools::check()        # or:  R CMD check .
