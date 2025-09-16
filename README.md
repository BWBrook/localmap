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

- `cfg`: configuration list
- `raw_manifest`: parsed input manifest
- `input_files`: file paths tracked as dependencies (format = "file")
- `data_preview`: lightweight preview of CSV inputs
- `combined_data`: combined tibble across CSV inputs (common columns)
- `manifest_summary`: counts and distinct file metrics
- `report`: renders `reports/paper.qmd`

See `docs/DEVELOPMENT.qmd` for project conventions and additional guidance.
