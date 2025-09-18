# localmap

Localmap bundles the scaffolding we need to source, clean, and map species occurrence
data. The repository keeps the reproducible tooling from the template but orients it
squarely around biodiversity workflows.

- Reproducible environment with `renv`
- Pipeline orchestrated by `{targets}`
- Tests with `{testthat}`
- Documentation via Quarto in `docs/` and long-form reports under `reports/`
  - Vignettes under `vignettes/`
  - Package site configured for `docs/pkgdown/`

Quick start (run in the RStudio Console):

```r
renv::restore(prompt = FALSE)
source("dependencies.R")
pak::pkg_install(project_dependencies())
devtools::document()
testthat::test_dir("tests/testthat", reporter = "summary")
```

Run the pipeline after restoring deps:

```r
targets::tar_make()
```

Render docs:

```bash
quarto render docs/
```

Build the pkgdown site (outputs to `docs/pkgdown/`):

```r
pkgdown::build_site(preview = FALSE)
```

Browse the vignette:

```r
utils::browseVignettes(package = "localmap")
```

Key targets available out of the box:

- `ala_config`: galah configuration derived from `config/config.yaml`
- `species_requests`: parsed species query definitions (`metadata/species_requests.csv`)
- `species_taxa`: resolved species metadata from the Atlas of Living Australia
- `species_occurrences`: occurrence records retrieved via galah
- `species_map`: static map image highlighting occurrence points within the
  configured bounding box
- `topo_tile_map`: tile-based map (defaults to `Esri.WorldImagery`, with
  alternatives like `OpenTopoMap`) overlaying species
  and ancillary points for the configured extent
- `camera_sites_tile`: basemap tile (WMS or placeholder) for the camera site extent.
- `camera_sites_map`: effort-scaled wildlife camera map highlighting detections
  (red) versus non-detections (light grey) and including scale/north markers.
- `cfg`: configuration list
- `raw_manifest`: parsed input manifest
- `input_files`: file paths tracked as dependencies (format = "file")
- `data_preview`: lightweight preview of CSV inputs
- `combined_data`: combined tibble across CSV inputs (common columns)
- `manifest_summary`: counts and distinct file metrics
- `report`: renders `reports/paper.qmd`

See `docs/DEVELOPMENT.qmd` for project conventions and additional guidance.


## Wildlife camera deployments

Provide wildlife camera effort/observation data via `config/config.yaml` →
`data$camera_sites`. The CSV is expected to contain `site`, `lat`, `lon`, `rd`
(run days), and `obs` (detections for the target species). The configuration
also selects a central site and half-width (km) to build a square bounding box,
plus an optional WMS provider for basemap tiles. Example:

```
data:
  camera_sites:
    path: data/raw/lyr_op.csv
    central_site: wc_s10_c5
    bbox_half_km: 20
    basemap:
      provider: list_tasveg
      type: wms
      url: https://services.thelist.tas.gov.au/arcgis/services/Public/NaturalEnvironment/MapServer/WMSServer
      layers: ["TASVEG_4.017789"]
      styles: default
      format: image/png
      transparent: true
      width: 2048
      height: 2048
      dpi: 96
      max_scale: 100000
      output_path: outputs/tiles/tasveg_camera_sites.png
    map:
      output_path: outputs/maps/camera_sites.png
      zero_obs_colour: "#cccccc"
      positive_obs_colour: "#d73027"
      show_graticule: true
      graticule_colour: "#050505"
      graticule_alpha: 0.4
      graticule_label_size: 3
      legend:
        show: true
        position: right
        layout: overlay
        width_fraction: 0.2
        margin_fraction: 0.05
```

The pipeline reads the CSV (`camera_sites_data`), prepares the spatial extent
(`camera_sites_context`), downloads or fabricates a tile (`camera_sites_tile`),
and renders the final map (`camera_sites_map`). Update the provider details to
match your LIST credentials or alternative WMS sources.

If the requested extent, dpi, and image size exceed the service visibility (≈1:100k for TASVEG), `prepare_camera_site_context()` automatically clamps the half-width to keep the request within range and the map warns when a tile would render empty. Graticule styling is configurable via `show_graticule` and related settings.

LIST TASVEG layers only render up to their published visibility scale (≈1:100k). Configure `max_scale` in `config.yaml` and keep the requested extent, image size, and DPI within that limit (e.g., halve `bbox_half_km`, reduce `dpi`, or increase `width`/`height`). When the limit is exceeded the WMS returns a fully transparent PNG, so the helper now warns if the request is out of range. Legend rendering (via WMS `GetLegendGraphic`) is on by default—set `map$legend$show = FALSE` to suppress it, or adjust `legend$layout` (`overlay` vs `sidebar`) and the width/margin fractions to control placement.

## Species occurrence retrieval

Provide species definitions in `metadata/species_requests.csv` with columns
`species_id`, `common_name`, and `scientific_name` (at least one of the names
must be supplied). The pipeline resolves those names against the Atlas of Living
Australia using `{galah}` and stores the results in the `species_occurrences`
target. Set the `ALA_EMAIL` environment variable (or edit
`config/config.yaml` → `ala$email`) so galah can authenticate with the API and
provide a valid `download_reason_id` (defaults to 4, “Scientific research”).
Configure temporal, jurisdiction, and optional latitude filters under
`config/config.yaml` → `data$occurrence` (e.g., `min_year`, `state_province`,
and `min_lat`). To generate maps, set `data$occurrence$map` with a bounding
box, for example:

```
data:
  occurrence:
    map:
      basemap: rnaturalearth
      show_title: false
      show_legend: false
      point_colour: "#2171b5"
      point_size: 1.6
      additional_points:
        path: data/raw/lyr_pt.csv
        colour: "#d73027"
        size: null
      # Tile-based target options
      tile_provider: Esri.WorldImagery  # alternatives include OpenTopoMap
      tile_zoom: 8
      tile_outline_colour: "#ffffff"
      tile_graticule: false
      topo_output_path: outputs/maps/species_topo.png
      bounding_box:
        top_left:
          lat: -40.2
          lon: 144.0
        length_km: 400
      output_path: outputs/maps/species_occurrences.png
```

The bounding box approach ensures the map covers Tasmania for the current
lyrebird case study while remaining configurable for future species. Provide
optional ancillary locations via `additional_points`; they should include
`lat`/`lon` columns in the referenced CSV and will render in the configured
colour (default red). Tile-based backgrounds for the `topo_tile_map` target can
be adjusted via `tile_provider` (e.g., `Esri.WorldImagery`, `OpenTopoMap`),
`tile_zoom`, `tile_outline_colour`, and `tile_graticule` (adds labelled degree
graticules). The rendered file defaults to `outputs/maps/species_topo.png` but
can be overridden with
`topo_output_path`.
