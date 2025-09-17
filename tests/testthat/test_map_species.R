test_that("map_species_occurrences creates an image file", {
  records <- tibble::tibble(
    species_id = "species_01",
    matched_common_name = "Superb Lyrebird",
    matched_scientific_name = "Menura novaehollandiae",
    query_common_name = "Superb Lyrebird",
    query_scientific_name = "Menura novaehollandiae",
    longitude = c(146.5, 147.1),
    latitude = c(-41.8, -42.2)
  )

  tmp_path <- file.path(tempdir(), paste0("map-", as.integer(runif(1, 1, 1e6)), ".png"))
  extra_path <- file.path(tempdir(), paste0("extra-", as.integer(runif(1, 1, 1e6)), ".csv"))
  utils::write.csv(
    data.frame(lat = c(-41.6, -42.1), lon = c(146.8, 147.3)),
    extra_path,
    row.names = FALSE
  )
  mapping <- list(
    bounding_box = list(
      top_left = list(lat = -40.2, lon = 144.0),
      length_km = 400
    ),
    basemap = "none",
    output_path = tmp_path,
    additional_points = list(path = extra_path, colour = "#ff0000", size = 1.2)
  )

  result <- expect_no_warning(map_species_occurrences(records, mapping_settings = mapping))
  expect_true(file.exists(result))
  expect_identical(normalizePath(result, winslash = "/", mustWork = TRUE),
                   normalizePath(tmp_path, winslash = "/", mustWork = TRUE))
})

test_that("map_species_occurrences errors when bounding box missing", {
  records <- tibble::tibble(
    species_id = "species_01",
    longitude = 147,
    latitude = -42
  )

  expect_error(
    map_species_occurrences(records, mapping_settings = list()),
    class = "map_species_occurrences_no_bbox"
  )
})

test_that("missing additional points file triggers warning but continues", {
  records <- tibble::tibble(
    species_id = "species_01",
    matched_common_name = "Superb Lyrebird",
    matched_scientific_name = "Menura novaehollandiae",
    query_common_name = "Superb Lyrebird",
    query_scientific_name = "Menura novaehollandiae",
    longitude = 147,
    latitude = -42
  )

  tmp_path <- file.path(tempdir(), paste0("map-", as.integer(runif(1, 1, 1e6)), ".png"))
  mapping <- list(
    bounding_box = list(
      top_left = list(lat = -40.2, lon = 144.0),
      length_km = 400
    ),
    basemap = "none",
    output_path = tmp_path,
    additional_points = list(path = file.path(tempdir(), "missing.csv"))
  )

  expect_warning(
    result <- map_species_occurrences(records, mapping_settings = mapping),
    regexp = "Additional points file not found"
  )
  expect_true(file.exists(result))
})
