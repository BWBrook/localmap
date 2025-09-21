# AGENT NOTES

- 2025-09-19: Extended camera site panel stacking.
  - Enhanced `camera_sites_stack_maps()` to deduplicate glob/directories, accept
    explicit grid sizing via `columns`/`rows`, honour regex-based `order`, and
    render configurable bottom-right labels (`a)`, `b)`, `c)`, …).
  - Configuration now accepts `input_dir`, regex `pattern`, ordering, and label
    settings; README/PIPELINE/CHANGELOG updated accordingly with overlay examples.
  - Tests expanded for duplicate handling, layout overrides, ordering, and label
    rendering.

- 2025-09-18: Hardened camera site basemap fetch against WMS scale limits.
  - Auto-clamp camera map half-width to comply with LIST TASVEG visibility scaling, added graticule styling controls, WMS overlay support (e.g., roads/towns), and a configurable legend inset.
  - Added `max_scale` config knob (default 1:100k) and scale diagnostics in `fetch_camera_wms_tile()`; default DPI now 96 to keep 40 km windows renderable.

- 2025-09-18: Added camera site mapping workflow for wildlife camera deployments.
  - Introduced `camera_sites_default()` helpers, camera site readers, context
    preparation, WMS tile fetching with placeholder handling, and map rendering
    with scale bar/north arrow.
  - Added config block (`data$camera_sites`) plus new targets to `_targets.R` for
    CSV tracking, WMS tile acquisition, and map generation.
  - Created tests covering the camera site helpers and updated docs/changelog
    to describe the new pipeline branch.

- 2025-09-17: Added mapping workflow for occurrence records.
  - Introduced `map_species_occurrences()` helper with bounding box logic and
    new dependencies (`ggplot2`, `geosphere`, `rnaturalearth`, `rnaturalearthdata`).
  - Extended configuration (`data$occurrence$map`) with Tasmania-focused bounding
    box defaults and wired `species_map` target into `_targets.R`.
  - Documented the new target and config in README and pipeline notes; added
    tests covering map generation.
  - Added optional Google terrain/satellite basemaps via `{ggmap}` with a plain
    fallback, configurable point styling (colour/size/species legend), Natural
    Earth land shading as the default, support for OpenStreetMap/Stamen tiles,
    and removal of the on-plot bounding box.
  - Added `src/basemap_debug.R` to run manual basemap diagnostics outside the
    `{targets}` pipeline (saves previews + summary under
    `outputs/scratch/basemap_debug/`).

- 2025-09-17: Restored mapping workflow for occurrence records after rollback.
  - Reintroduced `map_species_occurrences()` helper, `species_map` target, and
    supporting configuration/docs/tests aligned with the original approach.
  - Tweaked defaults to remove the bounding box outline, hide title/legend via
    config flags, and render smaller green points by default. Added optional
    CSV-driven ancillary points (defaulting to red) layered on the map, plus a
    new `topo_tile_map` target using slippy tiles (OpenTopoMap by default).

- 2025-09-16: Rebranded template to the localmap species occurrence project.
  - Renamed package metadata, Quarto, and pkgdown references to `localmap`.
  - Updated README, docs, and vignette language for species mapping context.
  - Renamed the RStudio project file to `localmap.Rproj` and refreshed ignores/changelog.
  - Added ALA workflow helpers (`read_species_requests()`, `ala_resolve_species()`,
    `ala_fetch_occurrences()`) plus supporting tests/config/docs; introduced
    `metadata/species_requests.csv` sample input.
  - Extended occurrence configuration with minimum latitude filtering and
    hardened galah downloads to handle varying return types.
  - Added state province filtering and enforced galah `download_reason_id`
    configuration (defaulting to scientific research).
  - **Important:** galah 2.x only accepts literal pipelines for filters. The
    downloader builds a single string (`galah_call() |> ...`) and evaluates it.
    Do not replace this with tidy-eval/quosure logic unless galah relaxes that
    restriction.
  - Added `scripts/ala_scratch.R` to debug galah calls outside the pipeline.
- 2025-09-12: Genericized template per AGENTS.md.
  - Rename package to `rcompendium`; update README and Quarto title.
  - Add `cfg_read()` and `read_manifest()` helpers; create tests.
  - Update `_targets.R` with `input_files`, `data_preview`, `manifest_summary`; fix report target.
  - Add sample data and update manifest.
  - Add `.Rproj`, `.Rbuildignore`, CI workflow, and docs scaffolding.
  - Clean `.gitignore`; fix profiles to use local SLURM template.
  - Add vignette placeholder, docs index, and Makefile.
  - Add `_pkgdown.yml` and `combined_data` target with reader helper.
