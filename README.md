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
- `cfg`: configuration list
- `raw_manifest`: parsed input manifest
- `input_files`: file paths tracked as dependencies (format = "file")
- `data_preview`: lightweight preview of CSV inputs
- `combined_data`: combined tibble across CSV inputs (common columns)
- `manifest_summary`: counts and distinct file metrics
- `report`: renders `reports/paper.qmd`

See `docs/DEVELOPMENT.qmd` for project conventions and additional guidance.

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
and `min_lat`).
