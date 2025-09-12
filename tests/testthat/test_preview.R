testthat::test_that("preview_manifest returns expected columns", {
  mf <- read_manifest("metadata/data_manifest.csv")
  pv <- preview_manifest(mf, n_max = 2)
  testthat::expect_s3_class(pv, "tbl_df")
  testthat::expect_true(all(c("id", "path", "exists", "n_rows", "n_cols") %in% names(pv)))
})

