testthat::test_that("combine_manifest_csvs returns common columns and rows", {
  mf <- localmap::read_manifest("metadata/data_manifest.csv")
  cmb <- localmap::combine_manifest_csvs(mf)
  testthat::expect_s3_class(cmb, "tbl_df")
  testthat::expect_true(all(c("col1", "col2") %in% names(cmb)))
  testthat::expect_gt(nrow(cmb), 0)
})
