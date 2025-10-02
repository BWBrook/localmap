# AGENTS.md (R / Codex CLI)

## 0) Identity & Scope

* You are a **local, file-editing agent** operating only within this repository working tree, where you have full read-write permission and network access.
* You have full access to filesystem tools,  `Rscript`, `make`, `tar_make`, `testthat`, `quarto` etc. Use these to test, bugfix and iterate.
* **Local git only unless asked.** You may create local commits; only push, fetch, pull, or touch remotes, when asked or approved by the developer.

## 1) Repo Assumptions

* R toolchain: R ≥ 4.5; Windows/Linux/macOS supported (WSL OK).
* Produce a **fully reproducible**, modular R workflow.
* Dependency mgmt: `renv` (primary), `pak` (install speed), `devtools` (checks/build).
* Workflow: `{targets}` for pipelines; `testthat` for tests; `quarto` for docs, `make` for build.
* If missing, create: `docs/`, `docs/CHANGELOG.md`, `.Rbuildignore`, `.gitignore`, `.lintr`, `.Rproj` (if not present), and minimal CI scaffolding under `.github/workflows/` (do not enable secrets).

## 2) Hard Rules (enforced)

* Prefer `rg` for searching; never open, stream or edit `renv.lock`.
* For large files under `data/` or `data-raw/`, inspection is permitted via safe, read-only shell commands only (headers, schema, small samples, counts). Do not stream bulk contents. For deeper analysis, add a small R helper and ask the user to run it.
* Place small example data under `inst/extdata/`.

## 3) Directory & File Conventions

* Package layout (if a package): `DESCRIPTION`, `NAMESPACE` (generated from roxygen), `R/`, `man/`, `tests/testthat/`, `inst/`, `vignettes/`, `data-raw/`, `docs/`, `reports/paper.qmd`, `outputs/`, `scripts/bootstrap.R`, `metadata/data_manifest.csv`, and `_targets.R` at repo root.
* Non-package project: still use `R/` for modular functions, `_targets.R` at root, `tests/` for unit tests, Quarto in `docs/` or `vignettes/`.
* Always add or update:
  * `docs/CHANGELOG.md` using **Keep a Changelog** style; one entry per meaningful change.
  * `docs/AGENT_NOTES.md` with a brief session log of edits (who/what/why).

## 4) Dependency Policy

* Prefer CRAN packages; record in `DESCRIPTION` (if a package) or a plain `dependencies.R` bootstrap (if not).
* Add roxygen `@importFrom` or fully qualify calls (e.g., `dplyr::mutate`).

## 5) Coding Style & Static Quality

* **Explicit imports only** — Do **not** use `library()` or `require()` anywhere.
* Prefer single-function .R files for modularity unless minor helpers (e.g., `helper_utils.R`).
* Each function should be single-purpose; avoid code repetition.
* Use `import::from("pkg", fun1, fun2)` in every function.
* Pure functions in `R/`; avoid `<<-`. Use `withr::local_*` for temporal state.
* Style: use `styler` default; wrap ≤ 100 chars; snake_case for objects; S3/S4 consistent.
* Lint: ensure a project `.lintr` file; conform to `{lintr}` defaults.
* Errors via `rlang::abort()` with typed classes; use `cli::cli_*` for user-facing messages.
* Never call `setwd()`; always resolve paths with `here()`.
* Exported helpers require minimal Roxygen documentation.
* Use `tidyverse` for wrangling, `tidymodels` for modelling; snake_case throughout.

## 6) `{targets}` Workflow

* Keep `_targets.R` tight but well commented: libraries, `tar_option_set()`, and pipeline.
* Define functions in `R/`, **not** inline inside `_targets.R`.
* Source all helpers with `targets::tar_source("R")`.
* Prefer fully qualified calls or `import::from()` inside functions; avoid `import::here()` within `_targets.R`.
* Use `import::from("library", function)` syntax for importing external functions from CRAN packages when needed.
* Use deterministic seeds (`tar_option_set(seed = 1L)`).
* Use pattern-safe dynamic branching; don’t bake file paths—use `tar_target(..., format = "file")` where appropriate.
* When you modify the pipeline, update `docs/PIPELINE.md` with a DAG summary.
* Use detailed comments and clear comment-based separators in the pipeline to improve readability.
* All summaries & artefacts must be derived from upstream targets; do not create side effects.

## 7) Testing & Checks

* Place tests in `tests/testthat/`; one file per exported function group.
* Write table-driven tests; include failure cases (`expect_error`, `expect_warning`).
* Coverage: include at least smoke + edge + error paths.
* Keep tests fast; avoid loading large data from `data/` or `data-raw/`. Use `inst/extdata/` samples.

## 8) Quarto / Documentation

* Place long-form docs in `docs/` or `vignettes/` (`.qmd`); keep runnable chunks (`eval=TRUE`) but assume user renders.
* Keep a `docs/DEVELOPMENT.qmd` with setup notes and decisions.

## 9) Git Workflow (local only)

* Commit frequently with Conventional Commits. Prefix automated commits with `agent:`.

  * Examples: `agent: feat(pipeline): add dynamic branching for site-level models`
* **Never** push, fetch, or change remotes unless explicilty asked by the developer.
* If a new file tree is created, include an initial commit message summarising structure.

## 10) Changelog Discipline

* If you change behaviour, interfaces, targets, or dependencies, update `docs/CHANGELOG.md` immediately.
* Template entry:

```
## [Unreleased]
### Added
- …

### Changed
- …

### Fixed
- …
```

## 11) Reproducibility & Cross-Platform

* Use forward slashes in paths; never hard-code `C:\...`.
* Gate OS-specific code with `Sys.info()[["sysname"]]`.
* For random processes, set `set.seed()` in test helpers, not inside functions.
* For heavy tasks, prefer chunked or lazy targets and format hints (e.g., `format = "qs"` when used by the project).

## 12) Data & Privacy

* Place small, non-sensitive example data in `inst/extdata/`. Large or sensitive data must be user-provided at runtime via config (YAML/JSON) and excluded by `.gitignore`.
* Never print or diff files that plausibly contain secrets (e.g., `.Renviron`).
* For large files under `data/` or `data-raw/`, inspection is permitted but must be minimal: show headers, schema, small samples, and safe counts only. Do not stream bulk contents into the conversation. If deeper inspection is needed, add a small R helper and ask the user to run it.

## 13) What to Create Automatically (if missing)

* `.gitignore` with: `.Rproj.user/`, `.Rhistory`, `.RData`, `renv/library/`, `_targets/`, `.quarto/`, `docs/_site/`.
* `.Rbuildignore` with: `_targets/`, `docs/`, `inst/extdata/cache/`, `.github/`, `.lintr`, `.quarto/`.
* `.lintr` with sensible defaults (line length, object length, cyclomatic complexity thresholds).
* `docs/` folder and `docs/PIPELINE.md`, `docs/AGENT_NOTES.md`.
* `docs/CHANGELOG.md` initialised if absent.

## 14) When Ambiguity Arises

Proceed without blocking, but follow this order:

1. Choose the **least destructive** change consistent with the codebase’s current conventions.
2. Leave an inline `# TODO(Codex):` with a short rationale and alternative considered.
3. Add a note in `docs/AGENT_NOTES.md`.
4. If the change would break public API, **stop and ask** for confirmation with a short diff and rollback plan.

## 15) Success Criteria

* No errors reported in R.
* All tests pass locally; `{targets}` completes cleanly on a fresh `renv::restore()`.
* Changelog updated; docs reflect pipeline and usage.
* No remote side-effects; repo remains buildable on Windows/macOS/Linux.
