test_that("read_camera_sites validates and loads data", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(
    tibble::tibble(
      site = c("centre", "inside"),
      lat = c(-41.0, -41.05),
      lon = c(145.0, 145.04),
      rd = c(100, 50),
      obs = c(1, 0)
    ),
    tmp
  )
  data <- read_camera_sites(tmp)
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 2L)
})

test_that("prepare_camera_site_context builds square bbox and filters", {
  data <- tibble::tibble(
    site = c("centre", "inside", "outside"),
    lat = c(-41.0, -41.05, -41.8),
    lon = c(145.0, 145.04, 145.9),
    rd = c(100, 50, 20),
    obs = c(1, 0, 0)
  )
  ctx <- prepare_camera_site_context(data, central_site = "centre", half_width_km = 20)
  expect_true(inherits(ctx$bbox_3857, "sfc_POLYGON"))
  bb <- sf::st_bbox(ctx$bbox_3857)
  expect_equal(unname(round((bb["xmax"] - bb["xmin"]) / 1000, 1)), 40.0)
  expect_equal(unname(round((bb["ymax"] - bb["ymin"]) / 1000, 1)), 40.0)
  expect_equal(sort(ctx$within$site), sort(c("centre", "inside")))
})

test_that("fetch_camera_wms_tile creates placeholder when URL missing", {
  data <- tibble::tibble(
    site = c("centre"),
    lat = c(-41.0),
    lon = c(145.0),
    rd = c(100),
    obs = c(1)
  )
  ctx <- prepare_camera_site_context(data, central_site = "centre", half_width_km = 5)
  tmp_tile <- file.path(tempdir(), "tile.png")
  if (file.exists(tmp_tile)) unlink(tmp_tile)
  tile_path <- fetch_camera_wms_tile(
    bbox_3857 = ctx$bbox_3857,
    provider_cfg = list(
      url = "",
      layers = "TASVEG",
      output_path = tmp_tile
    )
  )
  expect_equal(tile_path, tmp_tile)
  expect_true(file.exists(tmp_tile))
})

test_that("render_camera_sites_map writes map output", {
  data <- tibble::tibble(
    site = c("centre", "inside"),
    lat = c(-41.0, -41.05),
    lon = c(145.0, 145.04),
    rd = c(100, 40),
    obs = c(1, 0)
  )
  ctx <- prepare_camera_site_context(data, central_site = "centre", half_width_km = 10)
  tmp_tile <- file.path(tempdir(), "tile_render.png")
  if (file.exists(tmp_tile)) unlink(tmp_tile)
  invisible(fetch_camera_wms_tile(
    bbox_3857 = ctx$bbox_3857,
    provider_cfg = list(
      url = "",
      layers = "TASVEG",
      output_path = tmp_tile
    )
  ))
  tmp_map <- file.path(tempdir(), "camera_map.png")
  if (file.exists(tmp_map)) unlink(tmp_map)
  out <- render_camera_sites_map(
    context = ctx,
    tile_path = tmp_tile,
    map_cfg = list(
      output_path = tmp_map,
      scale_bar_location = "bl",
      north_arrow_location = "tr"
    )
  )
  expect_equal(out, tmp_map)
  expect_true(file.exists(tmp_map))
})
