testthat::test_that("cfg_read returns a list with default profile", {
  cfg <- cfg_read()
  testthat::expect_type(cfg, "list")
  testthat::expect_true(!is.null(cfg$data_dir))
})
