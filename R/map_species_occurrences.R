#' Map species occurrence records
#'
#' Generates a static map depicting species occurrence points within a user
#' defined bounding box. The bounding box is derived from configuration settings
#' (top-left coordinate and side length in kilometres). The output is written to
#' an image file to be tracked by the `{targets}` pipeline.
#'
#' @param occurrences Tibble or data frame of occurrence records containing at
#'   least `longitude`, `latitude`, and `species_id` columns.
#' @param mapping_settings Optional list with entries such as `bounding_box`,
#'   `basemap`, `output_path`, appearance toggles (`show_title`, `show_legend`),
#'   point styling, and an optional `additional_points` list containing `path`,
#'   `colour`, and `size`. Typically sourced from `config/config.yaml`.
#' @param output_path Optional override for the output image path. When `NULL`,
#'   the value from `mapping_settings$output_path` is used; if that is also
#'   missing, defaults to `here::here("outputs", "maps",
#'   "species_occurrences.png")`.
#' @return The path (character scalar) to the generated map image.
#' @export
map_species_occurrences <- function(occurrences,
                                    mapping_settings = NULL,
                                    output_path = NULL) {
  fn_env <- environment()
  import::from("checkmate", assert_data_frame, assert_list, assert_numeric, assert_string, .into = fn_env)
  import::from("dplyr", mutate, coalesce, .into = fn_env)
  import::from("geosphere", destPoint, .into = fn_env)
  import::from("ggplot2", coord_sf, geom_sf, geom_sf_text, ggplot, ggsave, labs, theme, theme_minimal, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("rnaturalearth", ne_states, .into = fn_env)
  import::from("readr", read_csv, .into = fn_env)
  import::from("rlang", abort, warn, .into = fn_env)
  import::from("sf", st_as_sf, st_bbox, st_crop, st_make_valid, st_polygon, st_sfc, st_within, st_filter, .into = fn_env)
  import::from("utils", modifyList, .into = fn_env)

  assert_data_frame(occurrences, min.rows = 0L)

  mapping_settings <- validate_mapping_settings(mapping_settings)
  defaults <- list(
    show_title = FALSE,
    show_legend = FALSE,
    point_colour = "#2171b5",
    point_size = 1.6,
    additional_points = list(
      path = NULL,
      colour = "#d73027",
      size = NULL
    )
  )
  mapping_settings <- modifyList(defaults, mapping_settings)

  assert_numeric(mapping_settings$point_size, len = 1L, lower = 0)
  assert_string(mapping_settings$point_colour, min.chars = 1L)

  additional_cfg <- mapping_settings$additional_points
  if (!is.null(additional_cfg$colour)) {
    assert_string(additional_cfg$colour, min.chars = 1L)
  }
  if (!is.null(additional_cfg$size)) {
    assert_numeric(additional_cfg$size, len = 1L, lower = 0)
  }
  additional_points_sf <- load_additional_points(additional_cfg)

  bbox_cfg <- mapping_settings$bounding_box
  if (is.null(bbox_cfg)) {
    abort(
      c(
        "A bounding box configuration is required for mapping.",
        i = "Set cfg$data$occurrence$map$bounding_box in config/config.yaml"
      ),
      class = "map_species_occurrences_no_bbox"
    )
  }

  top_left <- bbox_cfg$top_left
  if (is.null(top_left)) {
    abort("Bounding box must include a `top_left` coordinate.", class = "map_species_occurrences_missing_top_left")
  }
  top_left_vec <- coerce_top_left(top_left)

  length_km <- bbox_cfg$length_km
  assert_numeric(length_km, len = 1L, lower = 1e-6)
  side_length <- as.numeric(length_km)

  basemap <- mapping_settings$basemap
  if (is.null(basemap)) {
    basemap <- "rnaturalearth"
  }
  basemap <- match.arg(basemap, choices = c("rnaturalearth", "none"))

  if (is.null(output_path)) {
    output_path <- mapping_settings$output_path
  }
  if (is.null(output_path)) {
    output_path <- here::here("outputs", "maps", "species_occurrences.png")
  }
  assert_string(output_path, min.chars = 1L)

  coord_cols <- c("longitude", "latitude")
  if (!all(coord_cols %in% names(occurrences))) {
    abort(
      c(
        "Occurrence data must contain `longitude` and `latitude` columns.",
        i = "Ensure ala_fetch_occurrences() retains these columns."
      ),
      class = "map_species_occurrences_missing_coords"
    )
  }
  has_coords <- !is.na(occurrences$longitude) & !is.na(occurrences$latitude)
  occurrences <- occurrences[has_coords, , drop = FALSE]

  bbox <- build_square_bbox(top_left_vec, side_length)
  bbox_bb <- st_bbox(bbox)

  basemap_sf <- NULL
  if (identical(basemap, "rnaturalearth")) {
    basemap_sf <- get_basemap_tasmania(bbox)
  }

  occurrences_sf <- st_as_sf(
    occurrences,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

  if (nrow(occurrences_sf) > 0L) {
    occurrences_sf <- st_filter(occurrences_sf, bbox, .predicate = st_within)
  }

  if (!"species_id" %in% names(occurrences_sf)) {
    warn("`species_id` column missing; colouring by species will fallback to \"occurrence\".")
    occurrences_sf$species_id <- "occurrence"
  }

  occurrences_sf <- mutate(
    occurrences_sf,
    species_label = coalesce(
      matched_common_name,
      matched_scientific_name,
      query_scientific_name,
      query_common_name,
      species_id
    )
  )

  plot_obj <- ggplot()
  if (!is.null(basemap_sf)) {
    plot_obj <- plot_obj +
      geom_sf(data = basemap_sf, fill = "#f3f3f3", colour = "#b0b0b0", linewidth = 0.3)
  }
  if (nrow(occurrences_sf) > 0L) {
    plot_obj <- plot_obj +
      geom_sf(
        data = occurrences_sf,
        colour = mapping_settings$point_colour,
        alpha = 0.8,
        size = mapping_settings$point_size
      )
  }

  if (!is.null(additional_points_sf) && nrow(additional_points_sf) > 0L) {
    extra_size <- additional_cfg$size
    if (is.null(extra_size)) {
      extra_size <- mapping_settings$point_size
    }
    plot_obj <- plot_obj +
      geom_sf(
        data = additional_points_sf,
        colour = additional_cfg$colour,
        alpha = 0.9,
        size = extra_size
      )
  }

  plot_obj <- plot_obj +
    coord_sf(
      xlim = c(bbox_bb["xmin"], bbox_bb["xmax"]),
      ylim = c(bbox_bb["ymin"], bbox_bb["ymax"]),
      expand = FALSE
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = if (isTRUE(mapping_settings$show_legend)) "bottom" else "none")

  if (isTRUE(mapping_settings$show_title)) {
    plot_obj <- plot_obj +
      labs(
        title = "Species occurrences",
        subtitle = sprintf(
          "Bounding box top-left: (%.3f, %.3f); side: %.0f km",
          top_left_vec[["lat"]],
          top_left_vec[["lon"]],
          side_length
        ),
        x = "Longitude",
        y = "Latitude"
      )
  } else {
    plot_obj <- plot_obj + labs(x = "Longitude", y = "Latitude")
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename = output_path, plot = plot_obj, width = 7, height = 7, dpi = 300)

  output_path
}

validate_mapping_settings <- function(mapping_settings) {
  fn_env <- environment()
  import::from("checkmate", assert_list, .into = fn_env)

  if (is.null(mapping_settings)) {
    return(list())
  }
  assert_list(mapping_settings, any.missing = FALSE)
  mapping_settings
}

coerce_top_left <- function(value) {
  fn_env <- environment()
  import::from("checkmate", assert_numeric, .into = fn_env)

  if (is.list(value) && all(c("lat", "lon") %in% names(value))) {
    lat <- as.numeric(value$lat)
    lon <- as.numeric(value$lon)
    coords <- c(lat, lon)
  } else {
    coords <- as.numeric(unlist(value, use.names = FALSE))
  }
  assert_numeric(coords, len = 2L, any.missing = FALSE)
  c(lat = coords[[1]], lon = coords[[2]])
}

load_additional_points <- function(cfg) {
  fn_env <- environment()
  import::from("checkmate", assert_list, assert_character, assert_string, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("readr", read_csv, .into = fn_env)
  import::from("rlang", warn, .into = fn_env)
  import::from("sf", st_as_sf, .into = fn_env)

  if (is.null(cfg) || (!is.list(cfg))) {
    return(NULL)
  }

  path <- cfg$path
  if (is.null(path) || !is.character(path) || length(path) != 1L) {
    return(NULL)
  }

  candidate_paths <- unique(c(path, here(path)))
  resolved <- candidate_paths[file.exists(candidate_paths)][1]
  if (is.na(resolved)) {
    warn(sprintf("Additional points file not found: %s", path))
    return(NULL)
  }

  data <- try(read_csv(resolved, show_col_types = FALSE, progress = FALSE), silent = TRUE)
  if (inherits(data, "try-error")) {
    warn(sprintf("Failed to read additional points file: %s", resolved))
    return(NULL)
  }

  required <- c("lat", "lon")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    warn(sprintf("Additional points file missing columns: %s", paste(missing, collapse = ", ")))
    return(NULL)
  }

  data <- data[!is.na(data$lat) & !is.na(data$lon), , drop = FALSE]
  if (nrow(data) == 0L) {
    warn("Additional points file contains no valid lat/lon rows.")
    return(NULL)
  }

  st_as_sf(data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

create_graticule <- function(bbox) {
  fn_env <- environment()
  import::from("sf", st_as_sf, st_bbox, st_graticule, st_make_valid, st_segmentize, st_transform, .into = fn_env)

  bb <- st_bbox(bbox)
  lon_seq <- seq(floor(bb["xmin"]), ceiling(bb["xmax"]), by = 1)
  lat_seq <- seq(floor(bb["ymin"]), ceiling(bb["ymax"]), by = 1)

  if (length(lon_seq) < 2 && length(lat_seq) < 2) {
    return(NULL)
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
  grid_3857 <- st_intersection(st_transform(grid, 3857), bbox_3857)

  lon_offset <- (bb["xmax"] - bb["xmin"]) * 0.02
  lat_offset <- (bb["ymax"] - bb["ymin"]) * 0.02
  if (lon_offset == 0) lon_offset <- 0.01
  if (lat_offset == 0) lat_offset <- 0.01
  lat_label_offset <- (bb["xmax"] - bb["xmin"]) * 0.05
  if (lat_label_offset == 0) lat_label_offset <- 0.05

  format_lon <- function(x) {
    dir <- ifelse(x >= 0, "E", "W")
    sprintf("%.1f°%s", abs(x), dir)
  }
  format_lat <- function(y) {
    dir <- ifelse(y >= 0, "N", "S")
    sprintf("%.1f°%s", abs(y), dir)
  }

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
      lon = bb["xmin"] + lat_label_offset,
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

  list(lines = grid_3857, labels = labels_3857)
}

#' Render a tile-based occurrence map using slippy map tiles
#'
#' Fetches map tiles for the configured bounding box (defaulting to
#' OpenTopoMap) and overlays species occurrences plus any additional points.
#'
#' @inheritParams map_species_occurrences
#' @return Path to the rendered PNG file.
#' @export
generate_topo_tile_map <- function(occurrences,
                                   mapping_settings = NULL,
                                   output_path = NULL) {
  fn_env <- environment()
  import::from("checkmate", assert_data_frame, assert_logical, assert_numeric, assert_string, .into = fn_env)
  import::from("dplyr", mutate, coalesce, .into = fn_env)
  import::from("ggplot2", aes, coord_sf, geom_sf, geom_sf_text, ggplot, ggsave, labs, theme, theme_void, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("maptiles", get_tiles, .into = fn_env)
  import::from("rlang", warn, .into = fn_env)
  import::from("sf", st_as_sf, st_bbox, st_crs, st_filter, st_make_valid, st_transform, st_within, .into = fn_env)
  import::from("tidyterra", geom_spatraster_rgb, .into = fn_env)
  import::from("utils", modifyList, .into = fn_env)

  assert_data_frame(occurrences, min.rows = 0L)

  mapping_settings <- validate_mapping_settings(mapping_settings)
  defaults <- list(
    show_title = FALSE,
    show_legend = FALSE,
    point_colour = "#2ca25f",
    point_size = 1.6,
    additional_points = list(
      path = NULL,
      colour = "#d73027",
      size = NULL
    ),
    tile_provider = "Esri.WorldImagery",
    tile_zoom = 8,
    tile_outline_colour = "#ffffff",
    topo_output_path = NULL,
    tile_graticule = FALSE
  )
  mapping_settings <- modifyList(defaults, mapping_settings)

  assert_numeric(mapping_settings$point_size, len = 1L, lower = 0)
  assert_string(mapping_settings$point_colour, min.chars = 1L)
  assert_string(mapping_settings$tile_provider, min.chars = 1L)
  assert_numeric(mapping_settings$tile_zoom, len = 1L, lower = 0)
  assert_string(mapping_settings$tile_outline_colour, min.chars = 1L)
  assert_logical(mapping_settings$tile_graticule, len = 1L, any.missing = FALSE)

  additional_cfg <- mapping_settings$additional_points
  if (!is.null(additional_cfg$colour)) {
    assert_string(additional_cfg$colour, min.chars = 1L)
  }
  if (!is.null(additional_cfg$size)) {
    assert_numeric(additional_cfg$size, len = 1L, lower = 0)
  }

  additional_points_sf <- load_additional_points(additional_cfg)

  bbox_cfg <- mapping_settings$bounding_box
  if (is.null(bbox_cfg)) {
    warn("No bounding box configured; falling back to default species map.")
    return(map_species_occurrences(occurrences, mapping_settings, output_path))
  }

  top_left <- bbox_cfg$top_left
  if (is.null(top_left)) {
    warn("Bounding box missing top_left; falling back to default species map.")
    return(map_species_occurrences(occurrences, mapping_settings, output_path))
  }
  top_left_vec <- coerce_top_left(top_left)

  length_km <- bbox_cfg$length_km
  assert_numeric(length_km, len = 1L, lower = 1e-6)
  side_length <- as.numeric(length_km)

  if (is.null(output_path)) {
    output_path <- mapping_settings$topo_output_path
  }
  if (is.null(output_path)) {
    output_path <- here("outputs", "maps", "species_topo.png")
  }
  assert_string(output_path, min.chars = 1L)

  coord_cols <- c("longitude", "latitude")
  if (!all(coord_cols %in% names(occurrences))) {
    warn("Occurrence data missing longitude/latitude columns; falling back to default species map.")
    return(map_species_occurrences(occurrences, mapping_settings, output_path))
  }

  has_coords <- !is.na(occurrences$longitude) & !is.na(occurrences$latitude)
  occurrences <- occurrences[has_coords, , drop = FALSE]

  bbox <- build_square_bbox(top_left_vec, side_length)
  bbox_3857 <- st_transform(bbox, 3857)

  occurrences_sf <- st_as_sf(
    occurrences,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  if (nrow(occurrences_sf) > 0L) {
    occurrences_sf <- st_filter(occurrences_sf, bbox, .predicate = st_within)
  }
  occurrences_sf <- st_transform(occurrences_sf, 3857)

  additional_points_sf <- if (!is.null(additional_points_sf) && nrow(additional_points_sf) > 0L) {
    st_transform(additional_points_sf, 3857)
  } else {
    NULL
  }

  outline_sf <- get_basemap_tasmania(bbox)
  if (!is.null(outline_sf) && nrow(outline_sf) > 0L) {
    outline_sf <- st_transform(outline_sf, 3857)
  }

  tiles <- try(
    get_tiles(
      x = bbox_3857,
      provider = mapping_settings$tile_provider,
      zoom = as.integer(mapping_settings$tile_zoom),
      crop = TRUE
    ),
    silent = TRUE
  )
  if (inherits(tiles, "try-error")) {
    warn(sprintf(
      "Failed to retrieve tiles from provider '%s'; falling back to default species map.",
      mapping_settings$tile_provider
    ))
    return(map_species_occurrences(occurrences, mapping_settings, output_path))
  }

  plot_obj <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = tiles)

  if (!is.null(outline_sf) && nrow(outline_sf) > 0L) {
    plot_obj <- plot_obj +
      geom_sf(
        data = outline_sf,
        fill = NA,
        colour = mapping_settings$tile_outline_colour,
        linewidth = 0.4
      )
  }

  if (nrow(occurrences_sf) > 0L) {
    plot_obj <- plot_obj +
      geom_sf(
        data = occurrences_sf,
        colour = mapping_settings$point_colour,
        alpha = 0.8,
        size = mapping_settings$point_size
      )
  }

  if (!is.null(additional_points_sf) && nrow(additional_points_sf) > 0L) {
    extra_size <- additional_cfg$size
    if (is.null(extra_size)) {
      extra_size <- mapping_settings$point_size
    }
    plot_obj <- plot_obj +
      geom_sf(
        data = additional_points_sf,
        colour = additional_cfg$colour,
        alpha = 0.9,
        size = extra_size
      )
  }

  plot_obj <- plot_obj +
    coord_sf(crs = st_crs(3857), expand = FALSE)

  if (isTRUE(mapping_settings$tile_graticule)) {
    grid <- create_graticule(bbox)
    if (!is.null(grid)) {
      plot_obj <- plot_obj +
        geom_sf(
          data = grid$lines,
          colour = mapping_settings$tile_outline_colour,
          linewidth = 0.1,
          alpha = 0.6
        )
      if (!is.null(grid$labels) && nrow(grid$labels) > 0L) {
        plot_obj <- plot_obj +
          geom_sf_text(
            data = grid$labels,
            aes(label = label),
            colour = mapping_settings$tile_outline_colour,
            size = 2.8,
            check_overlap = TRUE
          )
      }
    }
  }

  plot_obj <- plot_obj + theme_void()

  if (isTRUE(mapping_settings$show_title)) {
    plot_obj <- plot_obj +
      labs(
        title = "Species occurrences",
        subtitle = sprintf(
          "Bounding box top-left: (%.3f, %.3f); side: %.0f km",
          top_left_vec[["lat"]],
          top_left_vec[["lon"]],
          side_length
        )
      )
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename = output_path, plot = plot_obj, width = 8, height = 8, dpi = 300)

  output_path
}

build_square_bbox <- function(top_left, side_length_km) {
  fn_env <- environment()
  import::from("geosphere", destPoint, .into = fn_env)
  import::from("sf", st_polygon, st_sfc, .into = fn_env)

  start <- c(lon = top_left[["lon"]], lat = top_left[["lat"]])
  metres <- side_length_km * 1000

  normalize_lonlat <- function(coord) {
    vals <- as.numeric(coord)
    if (length(vals) != 2L || any(is.na(vals))) {
      abort("destPoint returned invalid coordinates when constructing bounding box.", class = "map_species_occurrences_invalid_bbox")
    }
    structure(vals, names = c("lon", "lat"))
  }

  top_right <- normalize_lonlat(destPoint(start, b = 90, d = metres))
  bottom_left <- normalize_lonlat(destPoint(start, b = 180, d = metres))
  bottom_right <- normalize_lonlat(destPoint(bottom_left, b = 90, d = metres))

  coords <- rbind(
    c(start[["lon"]], start[["lat"]]),
    c(top_right[["lon"]], top_right[["lat"]]),
    c(bottom_right[["lon"]], bottom_right[["lat"]]),
    c(bottom_left[["lon"]], bottom_left[["lat"]]),
    c(start[["lon"]], start[["lat"]])
  )

  st_sfc(st_polygon(list(coords)), crs = 4326)
}

get_basemap_tasmania <- function(bbox) {
  fn_env <- environment()
  import::from("rnaturalearth", ne_states, .into = fn_env)
  import::from("sf", st_bbox, st_crop, st_make_valid, .into = fn_env)

  australia_states <- ne_states(country = "australia", returnclass = "sf")
  australia_states <- st_make_valid(australia_states)
  st_crop(australia_states, st_bbox(bbox))
}
