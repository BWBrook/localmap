fake_fetch <- function(identifier, species_row, min_year, boundary_wkt, columns, min_lat, state_province, config) {
  testthat::expect_equal(identifier, "Menura novaehollandiae")
  testthat::expect_equal(min_year, 1970)
  testthat::expect_equal(columns, c("decimalLongitude", "decimalLatitude"))
  testthat::expect_true(is.null(boundary_wkt) || is.character(boundary_wkt))
  tibble::tibble(
    longitude = 150.1,
    latitude = -33.9,
    eventDate = "2020-01-01",
    species_id = species_row$species_id[[1]],
    matched_scientific_name = species_row$matched_scientific_name[[1]],
    matched_common_name = species_row$matched_common_name[[1]],
    query_scientific_name = species_row$query_scientific_name[[1]],
    query_common_name = species_row$query_common_name[[1]],
    stateProvince = if (is.null(state_province)) "Tasmania" else state_province
  )
}

testthat::test_that("ala_fetch_occurrences iterates over taxa", {
  taxa <- tibble::tibble(
    species_id = "superb_lyrebird",
    query_common_name = "Superb Lyrebird",
    query_scientific_name = "Menura novaehollandiae",
    matched_scientific_name = "Menura novaehollandiae",
    matched_common_name = "Superb Lyrebird",
    match_type = "scientific_name"
  )
  records <- localmap::ala_fetch_occurrences(
    taxa = taxa,
    min_year = 1970,
    columns = c("decimalLongitude", "decimalLatitude"),
    fetch_fun = fake_fetch
  )
  testthat::expect_equal(nrow(records), 1L)
  testthat::expect_equal(records$species_id, "superb_lyrebird")
})

filter_fetch <- function(identifier, species_row, min_year, boundary_wkt, columns, min_lat, state_province, config) {
  out <- tibble::tibble(
    longitude = c(150.1, 150.2),
    latitude = c(-39.5, -41.2),
    eventDate = c("2020-01-01", "2020-01-02"),
    species_id = species_row$species_id[[1]],
    matched_scientific_name = species_row$matched_scientific_name[[1]],
    matched_common_name = species_row$matched_common_name[[1]],
    query_scientific_name = species_row$query_scientific_name[[1]],
    query_common_name = species_row$query_common_name[[1]],
    stateProvince = if (is.null(state_province)) "Tasmania" else state_province
  )
  if (!is.null(min_lat) && min_lat != 0) {
    out <- if (min_lat < 0) {
      dplyr::filter(out, latitude <= min_lat)
    } else {
      dplyr::filter(out, latitude >= min_lat)
    }
  }
  out
}

testthat::test_that("ala_fetch_occurrences applies minimum latitude filter", {
  taxa <- tibble::tibble(
    species_id = "superb_lyrebird",
    query_common_name = "Superb Lyrebird",
    query_scientific_name = "Menura novaehollandiae",
    matched_scientific_name = "Menura novaehollandiae",
    matched_common_name = "Superb Lyrebird",
    match_type = "scientific_name"
  )
  records <- localmap::ala_fetch_occurrences(
    taxa = taxa,
    min_lat = -40,
    state_province = "Tasmania",
    fetch_fun = filter_fetch
  )
  testthat::expect_equal(nrow(records), 1L)
  testthat::expect_true(all(records$latitude <= -40 | records$latitude >= 40))
})

testthat::test_that("ala_fetch_occurrences skips unresolved taxa", {
  taxa <- tibble::tibble(
    species_id = "unknown",
    query_common_name = "",
    query_scientific_name = NA_character_,
    matched_scientific_name = NA_character_,
    matched_common_name = NA_character_,
    match_type = "not_found"
  )
  records <- localmap::ala_fetch_occurrences(taxa, fetch_fun = fake_fetch)
  testthat::expect_equal(nrow(records), 0L)
})
