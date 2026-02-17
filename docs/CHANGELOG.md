# Changelog

All notable changes to this project will be documented in this file.

The format follows Keep a Changelog. This project adheres to Semantic Versioning where applicable.

## [Unreleased]
### Added
- `camera_sites_panel` target and stacking helper to combine multiple camera
  site map PNGs into a vertically aligned comparison panel.
- Panel stacking now deduplicates glob/dir matches, supports up to nine panes in
  configurable grids (via `stack$columns`/`rows`), honours regex ordering, and
  provides configurable bottom-right panel labels via
  `data$camera_sites$stack$labels`.
- Camera panel stacking now supports per-panel label colours and an optional
  centered footer image row (`stack$footer`), including direct WMS legend
  generation for larger manuscript-ready legend keys.
- Camera site mapping workflow: new configuration (`data$camera_sites`),
- Camera map now offers configurable graticules aligned to the bounding box extent, optional WMS overlay tiles (e.g., roads, towns), and a configurable legend inset.
- Camera site tile helper now warns when requested scale exceeds the LIST TASVEG visibility threshold; configuration adds `max_scale` and defaults to DPI 96.
  helper functions, and targets (`camera_sites_file`, `camera_sites_data`,
  `camera_sites_context`, `camera_sites_tile`, `camera_sites_map`) to visualise
  wildlife camera deployments with LIST TASVEG basemap support.
- Localmap branding and species occurrence framing across README and docs.
- Species request workflow (`read_species_requests()`, `ala_resolve_species()`,
  `ala_fetch_occurrences()`) to query the Atlas of Living Australia via
  `{galah}`.
- `metadata/species_requests.csv` sample input and config entries for Atlas
  access.
- Configuration knobs for occurrence filtering (`min_year`, `state_province`,
  optional `min_lat`, boundary geometry) surfaced in `config/config.yaml`, along
  with explicit galah `download_reason_id` support.
- Hard-code the galah pipeline as a literal string (`galah_call() |> ...`)
  because galah 2.x rejects dynamically constructed quosures; do not refactor
  to tidy-eval forms unless the upstream API changes.
- Static mapping utilities for occurrence points, including the
  `map_species_occurrences()` helper and `species_map` target that renders a
  bounding-box constrained image.
- Configuration entries for mapping (bounding box, basemap, output path) under
  `data$occurrence$map`.
- Optional ancillary point overlays drawn from CSV files (configured under
  `data$occurrence$map$additional_points`).
- Added `topo_tile_map` target to render OpenTopoMap-based tile imagery (or
  other providers) beneath species and ancillary points.
- Tile maps now support optional latitude/longitude graticule overlays with
  labelled degrees and default to blue occurrence markers for better contrast
  on satellite imagery.

### Changed
- Rename project metadata, Quarto site, and pkgdown home content from `rcompendium`
  to `localmap`.
- Update vignette examples and namespace-qualified calls to use `localmap::`.
- Refresh `DESCRIPTION` summary and `CITATION.cff` keywords to describe the
  species occurrence mapping focus.
- Expand `_targets.R` with ALA configuration, species resolution, and
  occurrence download targets.
- Default species map styling now hides the title/legend, removes the bounding
  box overlay, and uses smaller green points (configurable via
  `data$occurrence$map`).
- Camera panel stacking now excludes `stack$output_path` from discovered panel
  inputs to prevent accidental self-inclusion on reruns.
- Camera panel figure defaults now use larger panel labels (`font_size: 130`)
  and map subpanels for manuscript assembly are rendered without in-map TASVEG
  legend insets when a shared footer legend is used.

### Fixed
- Corrected the manually regenerated `camera_sites_mt_roland_m.png` basemap
  extent by using the intended Mt Roland center site (`vde`) and rebuilt the
  panel composite.
