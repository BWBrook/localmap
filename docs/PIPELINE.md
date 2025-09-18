# Pipeline Overview

Localmap uses the `{targets}` package to manage a reproducible pipeline for
species occurrence data products.

Current DAG (conceptual):
- `cfg` -> `ala_config`
- `cfg` -> `species_requests` -> `species_taxa` -> `species_occurrences` -> `species_map`
- `cfg` -> `raw_manifest` -> `input_files` -> `data_preview` -> `combined_data` -> `manifest_summary` -> `report`
- `cfg` -> `camera_sites_file` -> `camera_sites_data` -> `camera_sites_context` -> `camera_sites_tile` -> `camera_sites_map`

Key targets:
- `ala_config`: Configures `{galah}` using project settings or environment
  variables (`ALA_EMAIL`).
- `species_requests`: Reads `metadata/species_requests.csv` detailing the
  species (common and/or scientific names) to query.
- `species_taxa`: Resolves requested species via `galah::search_taxa()` to use
  canonical Atlas identifiers.
- `species_occurrences`: Downloads occurrence records for each resolved species
  and returns a tidy tibble with metadata (honouring configuration for minimum
  year, state province, latitude threshold, and optional boundary geometry).
- `species_map`: Produces a static map image using the configured bounding box,
  overlays the species occurrences, and can render supplementary point layers
  supplied via CSV (see `data$occurrence$map$additional_points`).
- `cfg`: Loads configuration from `config/config.yaml`; ensure the ALA section
  defines a valid email and `download_reason_id`.
- `raw_manifest`: Reads `metadata/data_manifest.csv` to enumerate input files.
- `input_files`: Tracks the files listed in the manifest as dependencies.
- `data_preview`: Lightweight summary (rows/columns) of CSV inputs.
- `combined_data`: Combined tibble across CSV inputs (common columns only).
- `manifest_summary`: Minimal summary of the manifest (counts and distinct files).
- `report`: Renders `reports/paper.qmd` to HTML.
- `topo_tile_map`: Renders a tile-based occurrence map (OpenTopoMap by default)
  using slippy tiles with optional ancillary points.
- `camera_sites_file`: Tracks the configured camera site CSV so changes rerun the
  camera mapping workflow.
- `camera_sites_data`: Reads the wildlife camera effort/observation table defined in
  `config$data$camera_sites$path`.
- `camera_sites_context`: Computes the square bounding box around the configured
  central site, automatically clamps the half-width to `data$camera_sites$basemap$max_scale`,
  and filters camera deployments in-range.
- `camera_sites_tile`: Downloads (or places a placeholder for) a WMS basemap tile
  from the configured provider (e.g., LIST TASVEG).
  The helper checks the requested scale against `max_scale` (default 1:100k for LIST) and warns if the tile would render empty.
- `camera_sites_map`: Renders an effort-scaled map with red markers for sites
  detecting the focal species and grey markers for zero detections, including
  scale bar, north arrow, optional graticules, and an optional WMS legend inset.

To run locally (in RStudio Console):

```r
renv::restore(prompt = FALSE)
source("dependencies.R")
pak::pkg_install(project_dependencies())
# Set profile via R_CONFIG_ACTIVE if desired (default is "default")
# Sys.setenv(R_CONFIG_ACTIVE = "local")

# Build pipeline
targets::tar_make()

# Visualise pipeline
# targets::tar_visnetwork()
```
