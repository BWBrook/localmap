#' Map species occurrence records
#'
#' Generates a static map depicting species occurrence points within a user
#' defined bounding box. The bounding box is derived from configuration settings
#' (top-left coordinate and side length in kilometres). The output is written to
#' an image file to be tracked by the `{targets}` pipeline.
#'
#' @param occurrences Tibble or data frame of occurrence records containing at
#'   least `longitude`, `latitude`, and `species_id` columns.
#' @param mapping_settings Optional list with `bounding_box`, `basemap`, and
#'   `output_path` entries, typically sourced from `config/config.yaml`.
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
  import::from("ggplot2", coord_sf, geom_sf, ggplot, ggsave, labs, theme, theme_minimal, .into = fn_env)
  import::from("rnaturalearth", ne_states, .into = fn_env)
  import::from("rlang", abort, warn, .into = fn_env)
  import::from("sf", st_as_sf, st_bbox, st_crop, st_make_valid, st_polygon, st_sfc, st_within, st_filter, .into = fn_env)
  import::from("utils", modifyList, .into = fn_env)

  assert_data_frame(occurrences, min.rows = 0L)

  mapping_settings <- validate_mapping_settings(mapping_settings)
  defaults <- list(
    show_title = FALSE,
    show_legend = FALSE,
    point_colour = "#2ca25f",
    point_size = 1.6
  )
  mapping_settings <- modifyList(defaults, mapping_settings)

  assert_numeric(mapping_settings$point_size, len = 1L, lower = 0)
  assert_string(mapping_settings$point_colour, min.chars = 1L)

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
