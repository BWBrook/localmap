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
                                        half_width_km,
                                        basemap_cfg = NULL) {
  fn_env <- environment()
  import::from("checkmate", assert_data_frame, assert_numeric, assert_string, .into = fn_env)
  import::from("rlang", abort, warn, .into = fn_env)
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

  effective_half_km <- as.numeric(half_width_km)
  clamp_note <- NULL
  if (!is.null(basemap_cfg)) {
    width_px <- camera_sites_default(basemap_cfg$width, 2048)
    dpi <- camera_sites_default(basemap_cfg$dpi, 96)
    max_scale <- basemap_cfg$max_scale
    if (is.null(max_scale)) {
      max_scale <- NA_real_
    }
    if (is.finite(width_px) && width_px > 0 && is.finite(dpi) && dpi > 0 && isTRUE(!is.na(max_scale)) && max_scale > 0) {
      max_total_width_m <- (as.numeric(width_px) * as.numeric(max_scale) * 0.0254) / as.numeric(dpi)
      allowed_half_km <- max_total_width_m / 2 / 1000
      if (is.finite(allowed_half_km) && allowed_half_km > 0 && effective_half_km > allowed_half_km) {
        clamp_note <- sprintf("Requested half-width %.1f km exceeds max-scale allowance %.1f km; clamping.", effective_half_km, allowed_half_km)
        effective_half_km <- allowed_half_km
      }
    }
  }
  if (!is.null(clamp_note)) {
    warn(clamp_note)
  }
  half_m <- effective_half_km * 1000

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
    centre_point_4326 = centre_point,
    half_width_km = effective_half_km,
    basemap_cfg = if (is.null(basemap_cfg)) list() else basemap_cfg
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


camera_sites_fetch_legend <- function(basemap_cfg, legend_cfg) {
  fn_env <- environment()
  import::from("checkmate", assert_list, assert_string, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("httr2", request, req_url_query, req_perform, resp_body_raw, resp_check_status, .into = fn_env)
  import::from("rlang", warn, .into = fn_env)

  if (is.null(legend_cfg) || !isTRUE(legend_cfg$show)) {
    return(NULL)
  }
  if (is.null(basemap_cfg)) {
    warn("Legend requested but basemap configuration missing; skipping legend fetch.")
    return(NULL)
  }
  url <- basemap_cfg$url
  layers <- basemap_cfg$layers
  if (is.null(url) || !nzchar(url) || is.null(layers) || length(layers) == 0L) {
    warn("Legend requested but WMS URL or layer not supplied; skipping legend fetch.")
    return(NULL)
  }
  layer <- trimws(as.character(layers[[1]]))
  if (!nzchar(layer)) {
    warn("Legend requested but first WMS layer is empty; skipping legend fetch.")
    return(NULL)
  }
  path <- basemap_cfg$legend_path
  if (is.null(path) || !nzchar(path)) {
    path <- here("outputs", "tiles", "camera_sites_legend.png")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  version <- camera_sites_default(basemap_cfg$version, "1.3.0")
  format <- camera_sites_default(legend_cfg$format, "image/png")
  styles <- camera_sites_default(basemap_cfg$styles, "")
  width <- legend_cfg$graphic_width
  height <- legend_cfg$graphic_height

  query <- list(
    service = "WMS",
    request = "GetLegendGraphic",
    version = version,
    layer = layer,
    format = format
  )
  if (!is.null(styles) && nzchar(styles)) {
    query$style <- styles
  }
  if (!is.null(width) && is.finite(width)) {
    query$width <- as.integer(width)
  }
  if (!is.null(height) && is.finite(height)) {
    query$height <- as.integer(height)
  }

  resp <- try(req_url_query(request(url), !!!query) |> req_perform(), silent = TRUE)
  if (inherits(resp, "try-error")) {
    warn("Failed to fetch WMS legend; legend will be omitted.")
    return(NULL)
  }
  resp_check_status(resp)
  body <- resp_body_raw(resp)
  if (length(body) == 0L) {
    warn("Empty WMS legend response; legend will be omitted.")
    return(NULL)
  }
  png_signature <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))
  is_png <- length(body) >= length(png_signature) && identical(body[seq_along(png_signature)], png_signature)
  if (!is_png) {
    warn("Legend response was not a PNG; legend will be omitted.")
    return(NULL)
  }
  writeBin(body, path)
  path
}


camera_sites_fetch_overlay_tiles <- function(bbox_3857,
                                             overlays,
                                             base_cfg) {
  fn_env <- environment()
  import::from("here", here, .into = fn_env)
  if (is.null(overlays) || length(overlays) == 0L) {
    return(list())
  }
  results <- list()
  for (idx in seq_along(overlays)) {
    overlay_cfg <- overlays[[idx]]
    if (is.null(overlay_cfg)) {
      next
    }
    if (is.null(overlay_cfg$url) || is.null(overlay_cfg$layers)) {
      next
    }
    merged <- overlay_cfg
    merged$width <- camera_sites_default(overlay_cfg$width, camera_sites_default(base_cfg$width, 2048L))
    merged$height <- camera_sites_default(overlay_cfg$height, camera_sites_default(base_cfg$height, 2048L))
    merged$dpi <- camera_sites_default(overlay_cfg$dpi, camera_sites_default(base_cfg$dpi, 96))
    merged$max_scale <- camera_sites_default(overlay_cfg$max_scale, camera_sites_default(base_cfg$max_scale, NA_real_))
    merged$output_path <- camera_sites_default(overlay_cfg$output_path, here::here("outputs", "tiles", sprintf("camera_sites_overlay_%02d.png", idx)))
    path <- fetch_camera_wms_tile(bbox_3857 = bbox_3857, provider_cfg = merged)
    if (!is.null(path) && nzchar(path) && file.exists(path)) {
      results[[length(results) + 1L]] <- list(
        path = path,
        opacity = camera_sites_default(overlay_cfg$opacity, 0.6)
      )
    }
  }
  results
}
render_camera_sites_map <- function(context,
                                    tile_path = NULL,
                                    map_cfg = NULL) {
  fn_env <- environment()
  import::from("checkmate", assert_list, .into = fn_env)
  import::from("ggplot2", annotation_raster, coord_sf, geom_sf, geom_sf_text, ggplot, ggsave, scale_colour_identity, scale_fill_identity, scale_size_identity, theme_void, .into = fn_env)
  import::from("ggspatial", annotation_north_arrow, annotation_scale, north_arrow_orienteering, .into = fn_env)
  import::from("grid", rasterGrob, gpar, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("png", readPNG, .into = fn_env)
  import::from("rlang", warn, .into = fn_env)
  import::from("sf", st_bbox, st_crs, .into = fn_env)
  import::from("utils", modifyList, .into = fn_env)

  assert_list(context, any.missing = FALSE)

  defaults <- list(
    output_path = here("outputs", "maps", "camera_sites.png"),
    scale_bar_location = "tl",
    north_arrow_location = "tr",
    point_size_min = 2,
    point_size_max = 8,
    zero_obs_colour = "#cccccc",
    positive_obs_colour = "#d73027",
    show_graticule = TRUE,
    graticule_colour = "#050505",
    graticule_alpha = 0.4,
    graticule_label_size = 3,
    legend = list(
      show = TRUE,
      position = "right",
      layout = "overlay",
      width_fraction = 0.2,
      margin_fraction = 0.05,
      format = "image/png",
      graphic_width = NULL,
      graphic_height = NULL
    )
  )
  map_cfg <- modifyList(defaults, camera_sites_default(map_cfg, list()))
  legend_cfg <- camera_sites_default(map_cfg$legend, list())
  legend_cfg$show <- isTRUE(camera_sites_default(legend_cfg$show, FALSE))
  legend_cfg$layout <- camera_sites_default(legend_cfg$layout, "overlay")
  legend_cfg$position <- camera_sites_default(legend_cfg$position, "right")
  legend_cfg$width_fraction <- as.numeric(camera_sites_default(legend_cfg$width_fraction, 0.2))
  legend_cfg$margin_fraction <- as.numeric(camera_sites_default(legend_cfg$margin_fraction, 0.05))

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

  legend_path <- NULL
  legend_grob <- NULL
  legend_dims <- NULL
  if (isTRUE(legend_cfg$show)) {
    legend_path <- camera_sites_fetch_legend(context$basemap_cfg, legend_cfg)
    if (!is.null(legend_path) && file.exists(legend_path)) {
      legend_img <- try(readPNG(legend_path), silent = TRUE)
      if (!inherits(legend_img, "try-error")) {
        legend_grob <- rasterGrob(legend_img, interpolate = TRUE)
        legend_dims <- dim(legend_img)
      } else {
        warn(sprintf("Failed to read legend image: %s", legend_path))
      }
    }
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

  overlays_cfg <- camera_sites_default(context$basemap_cfg$overlays, list())
  overlay_tiles <- camera_sites_fetch_overlay_tiles(context$bbox_3857, overlays_cfg, context$basemap_cfg)
  if (length(overlay_tiles) > 0L) {
    for (overlay in overlay_tiles) {
      overlay_img <- try(readPNG(overlay$path), silent = TRUE)
      if (!inherits(overlay_img, "try-error")) {
        opacity <- camera_sites_default(overlay$opacity, 0.6)
        overlay_grob <- rasterGrob(overlay_img, interpolate = TRUE, gp = grid::gpar(alpha = opacity))
        plot_obj <- plot_obj +
          ggplot2::annotation_custom(
            overlay_grob,
            xmin = bbox["xmin"],
            xmax = bbox["xmax"],
            ymin = bbox["ymin"],
            ymax = bbox["ymax"]
          )
      } else {
        warn(sprintf("Failed to read overlay image: %s", overlay$path))
      }
    }
  }

  if (isTRUE(map_cfg$show_graticule)) {
    grid <- camera_sites_create_graticule(context$bbox_4326)
    if (!is.null(grid$lines) && nrow(grid$lines) > 0L) {
      plot_obj <- plot_obj +
        geom_sf(
          data = grid$lines,
          colour = map_cfg$graticule_colour,
          alpha = map_cfg$graticule_alpha,
          linewidth = 0.2,
          inherit.aes = FALSE
        )
    }
    if (!is.null(grid$labels) && nrow(grid$labels) > 0L) {
      plot_obj <- plot_obj +
        geom_sf_text(
          data = grid$labels,
          ggplot2::aes(label = label),
          colour = map_cfg$graticule_colour,
          alpha = map_cfg$graticule_alpha,
          size = map_cfg$graticule_label_size,
          check_overlap = TRUE
        )
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

  coord_xlim_max <- bbox["xmax"]
  coord_ylim_min <- bbox["ymin"]
  coord_ylim_max <- bbox["ymax"]
  bbox_width <- bbox["xmax"] - bbox["xmin"]
  bbox_height <- bbox["ymax"] - bbox["ymin"]
  legend_xmin <- legend_xmax <- legend_ymin <- legend_ymax <- NA_real_
  if (!is.null(legend_grob) && identical(legend_cfg$position, "right")) {
    width_frac <- legend_cfg$width_fraction
    margin_frac <- legend_cfg$margin_fraction
    if (!is.finite(width_frac) || width_frac <= 0) width_frac <- 0.2
    if (!is.finite(margin_frac) || margin_frac < 0) margin_frac <- 0.05
    if (identical(legend_cfg$layout, "sidebar")) {
      legend_width <- bbox_width * width_frac
      legend_margin <- bbox_width * margin_frac
      coord_xlim_max <- bbox["xmax"] + legend_margin + legend_width
      legend_xmin <- bbox["xmax"] + legend_margin
      legend_xmax <- legend_xmin + legend_width
      legend_ymin <- bbox["ymin"] + bbox_height * margin_frac
      legend_ymax <- bbox["ymax"] - bbox_height * margin_frac
    } else {
      legend_width <- bbox_width * width_frac
      legend_height <- legend_width
      if (!is.null(legend_dims) && length(legend_dims) >= 2 && legend_dims[[2]] > 0) {
        aspect <- legend_dims[[1]] / legend_dims[[2]]
        legend_height <- legend_width * aspect
      }
      legend_margin <- bbox_width * margin_frac
      coord_xlim_max <- bbox["xmax"]
      legend_xmax <- bbox["xmax"] - legend_margin
      legend_xmin <- legend_xmax - legend_width
      legend_ymax <- bbox["ymax"] - bbox_height * margin_frac
      legend_ymin <- legend_ymax - legend_height
      min_y <- bbox["ymin"] + bbox_height * margin_frac
      if (legend_ymin < min_y) {
        shift <- min_y - legend_ymin
        legend_ymin <- legend_ymin + shift
        legend_ymax <- legend_ymax + shift
      }
      max_height <- bbox_height * (1 - 2 * margin_frac)
      if (legend_ymax - legend_ymin > max_height && max_height > 0) {
        legend_ymin <- legend_ymax - max_height
      }
    }
  }

  plot_obj <- plot_obj +
    coord_sf(
      crs = st_crs(3857),
      xlim = c(bbox["xmin"], coord_xlim_max),
      ylim = c(coord_ylim_min, coord_ylim_max),
      expand = FALSE
    ) +
    theme_void()

  if (!is.null(legend_grob) && !is.na(legend_xmin) && !is.na(legend_xmax)) {
    plot_obj <- plot_obj +
      ggplot2::annotation_custom(
        legend_grob,
        xmin = legend_xmin,
        xmax = legend_xmax,
        ymin = legend_ymin,
        ymax = legend_ymax
      )
  }

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



camera_sites_pick_step <- function(range_deg) {
  steps <- c(0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10)
  if (!is.finite(range_deg) || range_deg <= 0) {
    return(steps[[1]])
  }
  for (step in steps) {
    if ((range_deg / step) <= 8) {
      return(step)
    }
  }
  tail(steps, 1)
}

camera_sites_create_graticule <- function(bbox_4326) {
  fn_env <- environment()
  import::from("sf", st_as_sf, st_as_sfc, st_bbox, st_graticule, st_intersection, st_make_valid, st_segmentize, st_set_agr, st_sf, st_transform, .into = fn_env)

  if (is.null(bbox_4326)) {
    return(list(lines = NULL, labels = NULL))
  }
  bb <- st_bbox(bbox_4326)
  lon_range <- bb["xmax"] - bb["xmin"]
  lat_range <- bb["ymax"] - bb["ymin"]
  lon_step <- camera_sites_pick_step(lon_range)
  lat_step <- camera_sites_pick_step(lat_range)
  lon_seq <- seq(floor(bb["xmin"] / lon_step) * lon_step, ceiling(bb["xmax"] / lon_step) * lon_step, by = lon_step)
  lat_seq <- seq(floor(bb["ymin"] / lat_step) * lat_step, ceiling(bb["ymax"] / lat_step) * lat_step, by = lat_step)
  lon_seq <- unique(lon_seq)
  lat_seq <- unique(lat_seq)
  if (length(lon_seq) < 2 || length(lat_seq) < 2) {
    return(list(lines = NULL, labels = NULL))
  }

  grid <- st_graticule(
    lon = lon_seq,
    lat = lat_seq,
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"])
  )
  grid <- st_segmentize(grid, dfMaxLength = 25 * 1000)
  grid <- st_make_valid(grid)
  bbox_sfc <- st_as_sfc(bb)
  bbox_3857 <- st_transform(bbox_sfc, 3857)
  grid_3857 <- st_transform(grid, 3857)
  grid_3857 <- st_intersection(grid_3857, bbox_3857)
  grid_sf <- st_as_sf(grid_3857)
  grid_sf$id <- seq_len(nrow(grid_sf))
  grid_sf <- st_set_agr(grid_sf, "constant")
  row.names(grid_sf) <- NULL
  row.names(grid_sf) <- NULL

  format_lon <- function(x) {
    dir <- ifelse(x >= 0, "E", "W")
    sprintf("%.2f°%s", abs(x), dir)
  }
  format_lat <- function(y) {
    dir <- ifelse(y >= 0, "N", "S")
    sprintf("%.2f°%s", abs(y), dir)
  }

  lon_offset <- lon_range * 0.02
  lat_offset <- lat_range * 0.02
  if (!is.finite(lon_offset) || lon_offset <= 0) lon_offset <- 0.02
  if (!is.finite(lat_offset) || lat_offset <= 0) lat_offset <- 0.02

  lon_labels <- st_as_sf(
    data.frame(
      lon = lon_seq,
      lat = bb["ymin"] + lat_offset,
      label = format_lon(lon_seq)
    ),
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  lat_labels <- st_as_sf(
    data.frame(
      lon = bb["xmax"] - lon_offset,
      lat = lat_seq,
      label = format_lat(lat_seq)
    ),
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  labels <- rbind(lon_labels, lat_labels)
  labels <- st_intersection(labels, bbox_sfc)
  labels_3857 <- st_transform(labels, 3857)
  labels_3857 <- st_as_sf(labels_3857)
  labels_3857$id <- seq_len(nrow(labels_3857))
  labels_3857 <- st_set_agr(labels_3857, "constant")
  row.names(labels_3857) <- NULL

  list(lines = grid_sf, labels = labels_3857)
}
