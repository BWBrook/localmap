testthat::test_that("read_species_requests loads species definitions", {
  path <- "metadata/species_requests.csv"
  requests <- localmap::read_species_requests(path)
  testthat::expect_s3_class(requests, "tbl_df")
  testthat::expect_setequal(colnames(requests), c("species_id", "common_name", "scientific_name"))
  testthat::expect_true(all(requests$species_id != ""))
})

testthat::test_that("read_species_requests validates schema", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_lines("species_id\nmissing", file = tmp)
  testthat::expect_error(localmap::read_species_requests(tmp), class = "read_species_requests_schema")
})

testthat::test_that("read_species_requests requires a query term", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_lines("species_id,common_name,scientific_name\nsp1,,", file = tmp)
  testthat::expect_error(localmap::read_species_requests(tmp), class = "read_species_requests_no_query")
})
