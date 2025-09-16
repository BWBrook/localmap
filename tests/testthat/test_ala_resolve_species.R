fake_search <- function(term) {
  tibble::tibble(
    scientificName = c("Menura novaehollandiae", "Menura alberti"),
    vernacularName = c("Superb Lyrebird", "Albert's Lyrebird"),
    taxonConceptID = c("urn:lsid:biodiversity.org.au:afd.taxon:123", "urn:lsid:biodiversity.org.au:afd.taxon:456"),
    rank = c("species", "species"),
    search = term
  )
}

testthat::test_that("ala_resolve_species prefers scientific matches", {
  queries <- tibble::tibble(
    species_id = "superb_lyrebird",
    common_name = "Superb Lyrebird",
    scientific_name = "Menura novaehollandiae"
  )
  resolved <- localmap::ala_resolve_species(queries, search_fun = fake_search)
  testthat::expect_equal(resolved$matched_scientific_name, "Menura novaehollandiae")
  testthat::expect_equal(resolved$match_type, "scientific_name")
})

testthat::test_that("ala_resolve_species flags missing matches", {
  queries <- tibble::tibble(
    species_id = "unknown",
    common_name = NA_character_,
    scientific_name = "Not real"
  )
  empty_search <- function(term) tibble::tibble()
  resolved <- localmap::ala_resolve_species(queries, search_fun = empty_search)
  testthat::expect_true(is.na(resolved$matched_scientific_name))
  testthat::expect_equal(resolved$match_type, "not_found")
})
