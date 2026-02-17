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
      north_arrow_location = "tr",
      legend = list(show = FALSE)
    )
  )
  expect_equal(out, tmp_map)
  expect_true(file.exists(tmp_map))
})


test_that("prepare_camera_site_context clamps to max scale", {
  data <- tibble::tibble(
    site = c("centre", "inside"),
    lat = c(-41.0, -41.05),
    lon = c(145.0, 145.04),
    rd = c(100, 50),
    obs = c(1, 0)
  )
  basemap_cfg <- list(width = 1024, dpi = 254, max_scale = 100000)
  expect_warning(
    ctx <- prepare_camera_site_context(
      data,
      central_site = "centre",
      half_width_km = 50,
      basemap_cfg = basemap_cfg
    ),
    "clamping"
  )
  allowed_half_km <- ((1024 * 100000 * 0.0254) / 254) / 2 / 1000
  expect_equal(ctx$half_width_km, allowed_half_km)
  bb <- sf::st_bbox(ctx$bbox_3857)
  expect_equal(unname(round((bb["xmax"] - bb["xmin"]) / 1000, 3)), unname(round(allowed_half_km * 2, 3)))
})


test_that("camera_sites_stack_maps stacks PNGs vertically", {
  tmp_dir <- tempfile(pattern = "camera_stack")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  img_a <- array(1, dim = c(10, 20, 4))
  img_a[, , 1] <- 0.8
  img_a[, , 2] <- 0.2
  img_a[, , 3] <- 0.2
  img_a[, , 4] <- 1
  img_b <- array(1, dim = c(12, 18, 4))
  img_b[, , 1] <- 0.2
  img_b[, , 2] <- 0.8
  img_b[, , 3] <- 0.2
  img_b[, , 4] <- 1
  img_c <- array(1, dim = c(8, 22, 4))
  img_c[, , 1] <- 0.2
  img_c[, , 2] <- 0.2
  img_c[, , 3] <- 0.8
  img_c[, , 4] <- 1

  paths <- file.path(tmp_dir, c("a.png", "b.png", "c.png"))
  png::writePNG(img_a, target = paths[[1]])
  png::writePNG(img_b, target = paths[[2]])
  png::writePNG(img_c, target = paths[[3]])

  output_path <- file.path(tmp_dir, "stacked.png")
  res <- camera_sites_stack_maps(
    stack_cfg = list(
      paths = c(paths, paths),
      glob = file.path(tmp_dir, "*.png"),
      output_path = output_path,
      gap_px = 2,
      background = "#ffffff"
    ),
    prerequisites = NULL
  )

  expect_equal(as.character(res), output_path)
  expect_true(file.exists(as.character(res)))
  panel_paths <- attr(res, "panel_paths")
  expect_equal(length(panel_paths), 3L)
  expect_true(all(grepl("a|b|c", basename(panel_paths))))
  layout_info <- attr(res, "layout")
  expect_equal(layout_info$columns, 1L)
  expect_equal(layout_info$rows, 3L)

  stacked <- png::readPNG(as.character(res))
  panel_heights <- c(10, 12, 8)
  panel_widths <- c(20, 18, 22)
  cell_height <- max(panel_heights)
  cell_width <- max(panel_widths)
  gap_px <- 2
  columns <- 1L
  expected_width <- cell_width
  expected_height <- length(panel_heights) * cell_height + (length(panel_heights) - 1L) * gap_px

  expect_equal(dim(stacked)[2], expected_width)
  expect_equal(dim(stacked)[1], expected_height)

  tol <- 1e-6
  for (idx in seq_along(panel_heights)) {
    h <- panel_heights[[idx]]
    w <- panel_widths[[idx]]
    row_start <- (idx - 1L) * (cell_height + gap_px) + 1L
    col_start <- 1L
    row_offset <- floor((cell_height - h) / 2)
    col_offset <- floor((cell_width - w) / 2)
    rows <- (row_start + row_offset):(row_start + row_offset + h - 1L)
    cols <- (col_start + col_offset):(col_start + col_offset + w - 1L)
    col_target <- switch(
      idx,
      c(0.8, 0.2, 0.2),
      c(0.2, 0.8, 0.2),
      c(0.2, 0.2, 0.8)
    )
    for (channel in 1:3) {
      expect_true(
        all(abs(stacked[rows, cols, channel] - col_target[[channel]]) < tol),
        info = sprintf("channel %d mismatch for panel %d", channel, idx)
      )
    }
  }
})


test_that("camera_sites_stack_maps builds grids with labels", {
  tmp_dir <- tempfile(pattern = "camera_grid")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  cols <- list(
    c(0.9, 0.4, 0.4),
    c(0.4, 0.9, 0.4),
    c(0.4, 0.4, 0.9),
    c(0.9, 0.9, 0.4)
  )
  paths <- file.path(tmp_dir, sprintf("panel_%02d.png", seq_along(cols)))
  for (i in seq_along(cols)) {
    img <- array(1, dim = c(30, 30, 4))
    img[, , 1] <- cols[[i]][[1]]
    img[, , 2] <- cols[[i]][[2]]
    img[, , 3] <- cols[[i]][[3]]
    img[, , 4] <- 1
    png::writePNG(img, target = paths[[i]])
  }

  output_path <- file.path(tmp_dir, "stacked_grid.png")
  res <- camera_sites_stack_maps(
    stack_cfg = list(
      input_dir = tmp_dir,
      pattern = "panel_.*\\.png$",
      output_path = output_path,
      gap_px = 4,
      background = "#ffffff",
      labels = list(
        values = c("a)", "b)", "c)", "d)"),
        colour = "#000000",
        padding_px = 6,
        font_size = 14
      )
    ),
    prerequisites = NULL
  )

  expect_equal(as.character(res), output_path)
  expect_true(file.exists(as.character(res)))
  panel_paths <- basename(attr(res, "panel_paths"))
  expect_equal(panel_paths[1:3], sprintf("panel_%02d.png", 1:3))
  layout_info <- attr(res, "layout")
  expect_equal(layout_info$columns, 3L)
  expect_equal(layout_info$rows, 2L)

  stacked <- png::readPNG(as.character(res))
  cell_height <- 30
  cell_width <- 30
  expected_width <- 3 * cell_width + 2 * 4
  expected_height <- 2 * cell_height + 1 * 4
  expect_equal(dim(stacked)[2], expected_width)
  expect_equal(dim(stacked)[1], expected_height)

  label_positions <- list(
    c(1L, 1L),
    c(1L, 2L),
    c(1L, 3L),
    c(2L, 1L)
  )
  label_detected <- logical(length(label_positions))
  window <- 6
  for (i in seq_along(label_positions)) {
    pos <- label_positions[[i]]
    row_start <- (pos[[1]] - 1L) * (cell_height + 4) + 1L
    col_start <- (pos[[2]] - 1L) * (cell_width + 4) + 1L
    row_end <- row_start + cell_height - 1L
    col_end <- col_start + cell_width - 1L
    rows <- max(row_start, row_end - window + 1L):row_end
    cols <- max(col_start, col_end - window + 1L):col_end
    block <- stacked[rows, cols, 1:3]
    base_colour <- cols[[i]]
    label_detected[[i]] <- any(abs(block - base_colour) > 0.05)
  }
  expect_true(any(label_detected))
})


test_that("camera_sites_stack_maps honours layout overrides and ordering", {
  tmp_dir <- tempfile(pattern = "camera_layout")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  colours <- list(
    a = c(0.85, 0.15, 0.15),
    b = c(0.15, 0.85, 0.15),
    c = c(0.15, 0.15, 0.85)
  )
  paths <- file.path(tmp_dir, paste0("panel_", names(colours), ".png"))
  for (i in seq_along(paths)) {
    img <- array(1, dim = c(24, 30, 4))
    base <- colours[[i]]
    img[, , 1] <- base[[1]]
    img[, , 2] <- base[[2]]
    img[, , 3] <- base[[3]]
    img[, , 4] <- 1
    png::writePNG(img, target = paths[[i]])
  }

  output_path <- file.path(tmp_dir, "panel_order.png")
  res <- camera_sites_stack_maps(
    stack_cfg = list(
      input_dir = tmp_dir,
      pattern = "panel_.*\\.png$",
      order = c("panel_b", "panel_a"),
      columns = 2,
      rows = 2,
      gap_px = 5,
      output_path = output_path
    ),
    prerequisites = NULL
  )

  expect_equal(as.character(res), output_path)
  expect_true(file.exists(as.character(res)))

  layout_info <- attr(res, "layout")
  expect_equal(layout_info$columns, 2L)
  expect_equal(layout_info$rows, 2L)
  panel_paths <- basename(attr(res, "panel_paths"))
  expect_equal(panel_paths[1:3], c("panel_b.png", "panel_a.png", "panel_c.png"))
})

test_that("camera_sites_stack_maps supports per-panel label colours and footer rows", {
  tmp_dir <- tempfile(pattern = "camera_footer")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  panel_paths <- file.path(tmp_dir, c("left.png", "right.png"))
  for (i in seq_along(panel_paths)) {
    img <- array(1, dim = c(10, 10, 4))
    img[, , 1:3] <- if (i == 1L) 0.8 else 0.2
    img[, , 4] <- 1
    png::writePNG(img, target = panel_paths[[i]])
  }

  footer_path <- file.path(tmp_dir, "legend.png")
  footer_img <- array(1, dim = c(6, 12, 4))
  footer_img[, , 1] <- 0.95
  footer_img[, , 2] <- 0.95
  footer_img[, , 3] <- 0.95
  footer_img[, , 4] <- 1
  png::writePNG(footer_img, target = footer_path)

  output_path <- file.path(tmp_dir, "stacked_with_footer.png")
  res <- camera_sites_stack_maps(
    stack_cfg = list(
      paths = panel_paths,
      columns = 2,
      rows = 1,
      gap_px = 2,
      output_path = output_path,
      labels = list(
        values = c("a)", "b)"),
        colour = c("#050505", "#ffffff"),
        padding_px = 3,
        font_size = 10
      ),
      footer = list(
        show = TRUE,
        path = footer_path,
        padding_px = 3
      )
    ),
    prerequisites = NULL
  )

  expect_true(file.exists(as.character(res)))
  expect_equal(basename(attr(res, "footer_path")), "legend.png")

  stacked <- png::readPNG(as.character(res))
  expect_equal(dim(stacked)[2], 22)
  expect_equal(dim(stacked)[1], 19)
})

test_that("camera_sites_stack_maps excludes output file when matching inputs", {
  tmp_dir <- tempfile(pattern = "camera_exclude_output")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  img <- array(1, dim = c(8, 8, 4))
  img[, , 1] <- 0.6
  img[, , 2] <- 0.6
  img[, , 3] <- 0.6
  img[, , 4] <- 1
  png::writePNG(img, target = file.path(tmp_dir, "camera_sites_a.png"))
  png::writePNG(img, target = file.path(tmp_dir, "camera_sites_b.png"))

  output_path <- file.path(tmp_dir, "camera_sites_panel.png")
  png::writePNG(img, target = output_path)

  res <- camera_sites_stack_maps(
    stack_cfg = list(
      glob = file.path(tmp_dir, "camera_sites_*.png"),
      output_path = output_path,
      columns = 2,
      rows = 1
    ),
    prerequisites = NULL
  )

  expect_true(file.exists(as.character(res)))
  panel_paths <- basename(attr(res, "panel_paths"))
  expect_false("camera_sites_panel.png" %in% panel_paths)
  expect_equal(length(panel_paths), 2L)
})
