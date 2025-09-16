testthat::test_that("read_manifest reads and validates schema", {
  mf <- localmap::read_manifest("metadata/data_manifest.csv")
  testthat::expect_s3_class(mf, "tbl_df")
  testthat::expect_true(all(c("id", "path", "abs_path") %in% names(mf)))
  testthat::expect_gt(nrow(mf), 0)
})
