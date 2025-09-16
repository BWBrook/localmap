# Run tests for the localmap compendium

source("tests/testthat/helper-setup.R")
testthat::test_dir("tests/testthat", reporter = "summary")
