## localmap {targets} pipeline

targets::tar_option_set(
  seed = 1L,
  packages = character(0) # we use explicit namespace qualifiers/import::from within functions
)

# Source helpers
targets::tar_source("R")

list(
  # Configuration
  targets::tar_target(
    cfg_file,
    here::here("config", "config.yaml"),
    format = "file",
    description = "Path to the YAML configuration file (tracked for changes)."
  ),
  targets::tar_target(
    cfg,
    cfg_read(config_path = cfg_file),
    description = "Project configuration (paths, compute flags)."
  ),

  targets::tar_target(
    ala_config,
    ala_configure(cfg$ala),
    description = "Configured galah options for ALA access."
  ),

  targets::tar_target(
    species_requests,
    read_species_requests(cfg$data$species_requests),
    description = "Species queries supplied via CSV (common and scientific names)."
  ),

  targets::tar_target(
    species_taxa,
    ala_resolve_species(species_requests),
    description = "Resolved species metadata from the Atlas of Living Australia."
  ),

  targets::tar_target(
    species_occurrences,
    ala_fetch_occurrences(
      taxa = species_taxa,
      min_year = cfg$data$occurrence$min_year,
      boundary = cfg$data$occurrence$boundary,
      boundary_simplify = cfg$data$occurrence$boundary_simplify,
      columns = cfg$data$occurrence$fields,
      min_lat = cfg$data$occurrence$min_lat,
      state_province = cfg$data$occurrence$state_province,
      config = ala_config
    ),
    description = "Occurrence records retrieved from the Atlas of Living Australia."
  ),

  targets::tar_target(
    species_map,
    map_species_occurrences(
      occurrences = species_occurrences,
      mapping_settings = cfg$data$occurrence$map
    ),
    format = "file",
    description = "Static map visualising species occurrences within the configured bounding box."
  ),

  targets::tar_target(
    topo_tile_map,
    generate_topo_tile_map(
      occurrences = species_occurrences,
      mapping_settings = cfg$data$occurrence$map
    ),
    format = "file",
    description = "Tile-based occurrence map using slippy tiles (OpenTopoMap by default)."
  ),

  targets::tar_target(
    camera_sites_file,
    here::here(cfg$data$camera_sites$path),
    format = "file",
    description = "Camera site effort CSV defined in configuration."
  ),
  targets::tar_target(
    camera_sites_data,
    read_camera_sites(camera_sites_file),
    description = "Camera site metrics including effort and observations."
  ),
  targets::tar_target(
    camera_sites_context,
    prepare_camera_site_context(
      sites = camera_sites_data,
      central_site = cfg$data$camera_sites$central_site,
      half_width_km = cfg$data$camera_sites$bbox_half_km
    ),
    description = "Prepared spatial context (bounding box, filtered sites) for mapping camera deployments."
  ),
  targets::tar_target(
    camera_sites_tile,
    fetch_camera_wms_tile(
      bbox_3857 = camera_sites_context$bbox_3857,
      provider_cfg = cfg$data$camera_sites$basemap
    ),
    format = "file",
    description = "Basemap tile fetched from the configured WMS provider for camera sites."
  ),
  targets::tar_target(
    camera_sites_map,
    render_camera_sites_map(
      context = camera_sites_context,
      tile_path = camera_sites_tile,
      map_cfg = cfg$data$camera_sites$map
    ),
    format = "file",
    description = "Camera site map with effort-scaled symbols and observation highlighting."
  ),


  # Manifest of input files
  targets::tar_target(
    raw_manifest,
    read_manifest(cfg$data$manifest),
    description = "CSV manifest of input files."
  ),

  # Track file dependencies declared in the manifest (vector of paths)
  targets::tar_target(
    input_files,
    raw_manifest$abs_path,
    format = "file",
    description = "File paths declared in the manifest (tracked as file deps)."
  ),

  # Lightweight preview of input CSVs (row/column counts)
  targets::tar_target(
    data_preview,
    preview_manifest(raw_manifest, n_max = 100L),
    description = "Per-file preview (n_rows, n_cols) for CSV inputs."
  ),

  # Combine CSV inputs when columns align across files
  targets::tar_target(
    combined_data,
    combine_manifest_csvs(raw_manifest),
    description = "Combined tibble of CSV inputs using common columns."
  ),

  # Example summary derived from the manifest
  targets::tar_target(
    manifest_summary,
    {
      dplyr::summarise(
        raw_manifest,
        n_rows = dplyr::n(),
        n_files = dplyr::n_distinct(abs_path)
      )
    },
    description = "Simple summary of the manifest contents."
  ),

  # Report rendering: render to same directory as source file
  targets::tar_target(
    report,
    {
      qmd <- "reports/paper.qmd"
      out_dir <- dirname(qmd)
      # Render with Quarto if available; otherwise write a simple placeholder HTML
      has_quarto <- requireNamespace("quarto", quietly = TRUE) && !is.na(tryCatch(quarto::quarto_path(), error = function(e) NA))
      base <- tools::file_path_sans_ext(basename(qmd))
      html <- file.path(out_dir, paste0(base, ".html"))
      ok <- FALSE
      if (has_quarto) {
        ok <- isTRUE(tryCatch({
          quarto::quarto_render(input = qmd, quiet = TRUE)
          file.exists(html)
        }, error = function(e) FALSE))
      }
      if (!ok) {
        writeLines("<html><body><p>Report placeholder (Quarto not available or render failed).</p></body></html>", html)
      }
      html
    },
    format = "file",
    description = "Rendered Quarto report artifact."
  )
)
