#' Read camera site metrics from CSV
#'
#' @param path Character scalar. Path to the CSV containing columns `site`,
#'   `lat`, `lon`, `rd`, and `obs`.
#' @return Tibble with the expected columns.
camera_sites_default <- function(value, default) {
  if (is.null(value)) default else value
}

camera_sites_write_placeholder_tile <- function(path) {
  placeholder <- array(1, dim = c(1, 1, 4))
  placeholder[, , 4] <- 0
  png::writePNG(placeholder, path)
  path
}

read_camera_sites <- function(path) {
  fn_env <- environment()
  import::from("checkmate", assert_character, assert_file_exists, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("readr", read_csv, .into = fn_env)
  import::from("rlang", abort, .into = fn_env)
  import::from("tibble", as_tibble, .into = fn_env)

  assert_character(path, len = 1L, any.missing = FALSE)

  candidate_paths <- unique(c(path, here(path)))
  existing <- candidate_paths[file.exists(candidate_paths)]
  if (length(existing) == 0L) {
    abort(
      c(
        "Camera site CSV not found.",
        i = sprintf("Checked: %s", paste(candidate_paths, collapse = ", "))
      ),
      class = "read_camera_sites_missing_file"
    )
  }
  resolved <- existing[[1]]
  assert_file_exists(resolved)

  data <- read_csv(resolved, show_col_types = FALSE, progress = FALSE)
  required <- c("site", "lat", "lon", "rd", "obs")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    abort(
      c(
        "Camera site CSV missing required columns.",
        i = sprintf("Columns missing: %s", paste(missing, collapse = ", "))
      ),
      class = "read_camera_sites_missing_columns"
    )
  }

  as_tibble(data)
}

#' Build context for camera site mapping
#'
#' @param sites Tibble returned by `read_camera_sites()`.
#' @param central_site Character scalar identifying the focal site.
#' @param half_width_km Numeric scalar giving half the side length in km.
#' @return List containing filtered `sf` objects and bounding boxes in EPSG:4326
#'   and EPSG:3857.
prepare_camera_site_context <- function(sites,
                                        central_site,
                                        half_width_km) {
  fn_env <- environment()
  import::from("checkmate", assert_data_frame, assert_numeric, assert_string, .into = fn_env)
  import::from("rlang", abort, .into = fn_env)
  import::from("sf", st_as_sf, st_bbox, st_coordinates, st_crs, st_point, st_polygon, st_sfc, st_transform, st_within, .into = fn_env)

  assert_data_frame(sites, min.rows = 1L)
  assert_string(central_site, min.chars = 1L)
  assert_numeric(half_width_km, len = 1L, lower = 0)

  if (!central_site %in% sites$site) {
    abort(
      c(
        "Central site not found in camera site data.",
        i = sprintf("central_site = %s", central_site)
      ),
      class = "prepare_camera_site_missing_central"
    )
  }

  centre_row <- sites[sites$site == central_site, , drop = FALSE]
  centre_point <- st_sfc(st_point(c(centre_row$lon, centre_row$lat)), crs = 4326)
  centre_3857 <- st_transform(centre_point, 3857)
  coords_3857 <- as.numeric(st_coordinates(centre_3857))
  half_m <- half_width_km * 1000

  square_coords <- matrix(
    c(
      coords_3857[1] - half_m, coords_3857[2] + half_m,
      coords_3857[1] + half_m, coords_3857[2] + half_m,
      coords_3857[1] + half_m, coords_3857[2] - half_m,
      coords_3857[1] - half_m, coords_3857[2] - half_m,
      coords_3857[1] - half_m, coords_3857[2] + half_m
    ),
    byrow = TRUE,
    ncol = 2
  )
  bbox_3857 <- st_sfc(st_polygon(list(square_coords)), crs = 3857)
  bbox_4326 <- st_transform(bbox_3857, 4326)

  sites_sf <- st_as_sf(
    sites,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
  within_sf <- sites_sf[st_within(sites_sf, bbox_4326, sparse = FALSE), ]
  within_sf_3857 <- st_transform(within_sf, 3857)

  list(
    sites = sites_sf,
    within = within_sf_3857,
    bbox_4326 = bbox_4326,
    bbox_3857 = bbox_3857,
    centre_row = centre_row,
    centre_point_4326 = centre_point
  )
}

#' Download a WMS tile for the camera site extent
#'
#' @param bbox_3857 `sf` polygon (square) defining the bounding box in EPSG:3857.
#' @param provider_cfg List describing the WMS provider (url, layers, etc.).
#' @return Path to the tile file, or `NULL` when no provider is configured.
fetch_camera_wms_tile <- function(bbox_3857,
                                  provider_cfg) {
  fn_env <- environment()
  import::from("checkmate", assert_list, assert_string, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("httr2", request, req_url_query, req_perform, resp_body_raw, resp_check_status, .into = fn_env)
  import::from("rlang", warn, .into = fn_env)
  import::from("sf", st_bbox, st_crs, st_transform, .into = fn_env)

  default_output <- here("outputs", "tiles", "camera_sites_wms.png")
  make_placeholder <- function(path, message, detail = NULL) {
    note <- message
    if (!is.null(detail) && nzchar(detail)) {
      note <- sprintf("%s (%s)", message, detail)
    }
    warn(note)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    camera_sites_write_placeholder_tile(path)
  }

  if (is.null(provider_cfg)) {
    return(make_placeholder(default_output, "No WMS provider configured; creating placeholder tile."))
  }
  assert_list(provider_cfg, any.missing = FALSE)

  output_path <- camera_sites_default(provider_cfg$output_path, default_output)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  assert_string(output_path, min.chars = 1L)

  url <- provider_cfg$url
  if (is.null(url) || !nzchar(url)) {
    return(make_placeholder(output_path, "No WMS URL provided; creating placeholder tile."))
  }
  assert_string(url, min.chars = 1L)

  version <- camera_sites_default(provider_cfg$version, "1.3.0")
  assert_string(version, min.chars = 1L)

  layers <- provider_cfg$layers
  if (is.null(layers)) {
    return(make_placeholder(output_path, "No WMS layers configured; creating placeholder tile."))
  }
  if (is.character(layers)) {
    layers <- trimws(layers)
    layers <- layers[nzchar(layers)]
  }
  if (length(layers) == 0L) {
    return(make_placeholder(output_path, "No valid WMS layers supplied; creating placeholder tile."))
  }

  styles <- camera_sites_default(provider_cfg$styles, "")
  crs <- camera_sites_default(provider_cfg$crs, "EPSG:3857")
  assert_string(crs, min.chars = 1L)

  bbox_target <- st_bbox(st_transform(bbox_3857, st_crs(crs)))
  bbox_values <- as.numeric(bbox_target[c("xmin", "ymin", "xmax", "ymax")])
  if (utils::compareVersion(version, "1.3.0") >= 0 && startsWith(crs, "EPSG:4326")) {
    bbox_values <- bbox_values[c(2, 1, 4, 3)]
  }
  bbox_param <- paste(bbox_values, collapse = ",")

  width <- as.integer(camera_sites_default(provider_cfg$width, 2048L))
  height <- as.integer(camera_sites_default(provider_cfg$height, 2048L))
  fmt <- camera_sites_default(provider_cfg$format, "image/png")
  transparent <- provider_cfg$transparent
  if (is.null(transparent)) {
    transparent <- TRUE
  }
  dpi <- as.numeric(camera_sites_default(provider_cfg$dpi, 96))
  bbox_original <- st_bbox(bbox_3857)
  bbox_width_m <- as.numeric(bbox_original[["xmax"]] - bbox_original[["xmin"]])
  if (is.finite(bbox_width_m) && bbox_width_m > 0 && is.finite(dpi) && dpi > 0 && width > 0) {
    map_scale <- (bbox_width_m / width) * (dpi / 0.0254)
    max_scale <- camera_sites_default(provider_cfg$max_scale, NA_real_)
    if (isTRUE(!is.na(max_scale)) && map_scale > max_scale) {
      warn(sprintf("Requested WMS scale is approximately 1:%.0f which exceeds the configured limit (~1:%.0f). Consider reducing bbox_half_km, lowering dpi, or increasing image dimensions.", map_scale, max_scale))
    }
  }

  req <- request(url)
  param_names <- if (utils::compareVersion(version, "1.3.0") >= 0) {
    list(parameter_crs = "CRS")
  } else {
    list(parameter_crs = "SRS")
  }
  query <- list(
    service = "WMS",
    request = "GetMap",
    version = version,
    layers = paste(layers, collapse = ","),
    styles = styles,
    format = fmt,
    width = width,
    height = height,
    transparent = if (isTRUE(transparent)) "TRUE" else "FALSE"
  )
  query[[param_names$parameter_crs]] <- crs
  bbox_name <- "BBOX"
  query[[bbox_name]] <- bbox_param
  if (is.finite(dpi) && dpi > 0) {
    query$dpi <- dpi
  }

  resp <- try(req_url_query(req, !!!query) |> req_perform(), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(make_placeholder(output_path, "Failed to fetch WMS tile; using placeholder basemap."))
  }
  resp_check_status(resp)
  body <- resp_body_raw(resp)
  if (length(body) == 0L) {
    return(make_placeholder(output_path, "WMS response was empty; using placeholder basemap."))
  }
  png_signature <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  is_png <- length(body) >= length(png_signature) && identical(body[seq_along(png_signature)], png_signature)
  if (!is_png) {
    detail <- tryCatch({
      text_detail <- rawToChar(body)
      text_detail <- gsub("\\s+", " ", text_detail)
      substr(text_detail, 1, 160)
    }, error = function(e) "response not human-readable")
    return(make_placeholder(output_path, "WMS returned non-PNG content; using placeholder basemap.", detail))
  }
  writeBin(body, output_path)
  output_path
}


#' Render camera site map with effort-scaled points
#'
#' @param context List returned by `prepare_camera_site_context()`.
#' @param tile_path Optional path to a background tile image.
#' @param map_cfg List of display options (colours, output path, etc.).
#' @return Path to the rendered PNG file.
render_camera_sites_map <- function(context,
                                    tile_path = NULL,
                                    map_cfg = NULL) {
  fn_env <- environment()
  import::from("checkmate", assert_list, .into = fn_env)
  import::from("ggplot2", annotation_raster, coord_sf, geom_sf, ggplot, ggsave, scale_colour_identity, scale_fill_identity, scale_size_identity, theme_void, .into = fn_env)
  import::from("ggspatial", annotation_north_arrow, annotation_scale, north_arrow_orienteering, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("png", readPNG, .into = fn_env)
  import::from("rlang", warn, .into = fn_env)
  import::from("sf", st_bbox, st_crs, .into = fn_env)
  import::from("utils", modifyList, .into = fn_env)

  assert_list(context, any.missing = FALSE)

  defaults <- list(
    output_path = here("outputs", "maps", "camera_sites.png"),
    scale_bar_location = "bl",
    north_arrow_location = "tr",
    point_size_min = 2,
    point_size_max = 8,
    zero_obs_colour = "#cccccc",
    positive_obs_colour = "#d73027"
  )
  map_cfg <- modifyList(defaults, camera_sites_default(map_cfg, list()))

  bbox <- st_bbox(context$bbox_3857)
  within <- context$within
  if (nrow(within) == 0L) {
    warn("No camera sites fall within the configured bounding box.")
  }

  rd_values <- within$rd
  rd_values[!is.finite(rd_values)] <- NA_real_
  max_rd <- max(rd_values, na.rm = TRUE)
  if (!is.finite(max_rd) || max_rd <= 0) {
    point_sizes <- rep(map_cfg$point_size_min, length(rd_values))
  } else {
    scaled <- (rd_values / max_rd)
    scaled[is.na(scaled)] <- 0
    range_span <- map_cfg$point_size_max - map_cfg$point_size_min
    point_sizes <- map_cfg$point_size_min + (scaled * range_span)
  }

  plot_obj <- ggplot()

  if (!is.null(tile_path) && file.exists(tile_path)) {
    tile_img <- try(readPNG(tile_path), silent = TRUE)
    if (!inherits(tile_img, "try-error")) {
      plot_obj <- plot_obj +
        annotation_raster(
          tile_img,
          xmin = bbox["xmin"],
          xmax = bbox["xmax"],
          ymin = bbox["ymin"],
          ymax = bbox["ymax"],
          interpolate = TRUE
        )
    } else {
      warn(sprintf("Failed to read tile image: %s", tile_path))
    }
  }

  if (nrow(within) > 0L) {
    within$colour <- ifelse(within$obs > 0, map_cfg$positive_obs_colour, map_cfg$zero_obs_colour)
    within$size <- point_sizes
    plot_obj <- plot_obj +
      geom_sf(
        data = within,
        ggplot2::aes(colour = colour, fill = colour, size = size),
        alpha = 0.9,
        show.legend = FALSE
      ) +
      scale_colour_identity() +
      scale_fill_identity() +
      scale_size_identity()
  }

  plot_obj <- plot_obj +
    coord_sf(
      crs = st_crs(3857),
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    theme_void()

  plot_obj <- plot_obj +
    annotation_scale(
      location = map_cfg$scale_bar_location,
      style = "ticks",
      text_cex = 0.7
    ) +
    annotation_north_arrow(
      location = map_cfg$north_arrow_location,
      which_north = "true",
      style = north_arrow_orienteering(text_size = 6)
    )

  dir.create(dirname(map_cfg$output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename = map_cfg$output_path, plot = plot_obj, width = 7, height = 7, dpi = 300)
  map_cfg$output_path
}

