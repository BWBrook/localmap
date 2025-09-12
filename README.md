# rcompendium

Generic R compendium template for reproducible projects using renv, targets, and Quarto.

- Reproducible env with `renv`
- Pipeline with `{targets}`
- Tests with `{testthat}`
- Docs with Quarto under `docs/` and `reports/`
 - Vignettes under `vignettes/`
 - Package site with `pkgdown` (configured for `docs/pkgdown/`)

Quick start (in RStudio Console):

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
utils::browseVignettes(package = "rcompendium")
```

Key targets out of the box:

- `cfg`: configuration list
- `raw_manifest`: parsed input manifest
- `input_files`: file paths tracked as dependencies (format = "file")
- `data_preview`: lightweight preview of CSV inputs
- `combined_data`: combined tibble across CSV inputs (common columns)
- `manifest_summary`: counts and distinct file metrics
- `report`: renders `reports/paper.qmd`

See `docs/DEVELOPMENT.qmd` for notes and conventions.
