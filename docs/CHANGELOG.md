# Changelog

All notable changes to this project will be documented in this file.

The format follows Keep a Changelog. This project adheres to Semantic Versioning where applicable.

## [Unreleased]
### Added
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

### Fixed
- n/a
