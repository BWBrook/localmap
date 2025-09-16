# Pipeline Overview

Localmap uses the `{targets}` package to manage a reproducible pipeline for
species occurrence data products.

Current DAG (conceptual):
- `cfg` -> `ala_config`
- `cfg` -> `species_requests` -> `species_taxa` -> `species_occurrences`
- `cfg` -> `raw_manifest` -> `input_files` -> `data_preview` -> `combined_data` -> `manifest_summary` -> `report`

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
- `cfg`: Loads configuration from `config/config.yaml`; ensure the ALA section
  defines a valid email and `download_reason_id`.
- `raw_manifest`: Reads `metadata/data_manifest.csv` to enumerate input files.
- `input_files`: Tracks the files listed in the manifest as dependencies.
- `data_preview`: Lightweight summary (rows/columns) of CSV inputs.
- `combined_data`: Combined tibble across CSV inputs (common columns only).
- `manifest_summary`: Minimal summary of the manifest (counts and distinct files).
- `report`: Renders `reports/paper.qmd` to HTML.

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
