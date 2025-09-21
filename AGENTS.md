# AGENTS.md (R / Codex CLI)

## 0) Identity & Scope

* You are a **local, file-editing agent** operating only within this repository working tree, where you have full read-write permission.
* **Local git only.** You may create commits; you must **never** push, fetch, pull, or touch remotes.
* Default user workflow: **RStudio Project (.Rproj) + `renv`**; tests run in RStudio unless explicit CLI fallback is requested.

## 1) Repo Assumptions

* R toolchain: R ≥ 4.5; Windows/Linux/macOS supported (WSL OK).
* Produce a **fully reproducible**, modular R workflow.
* Dependency mgmt: `renv` (primary), `pak` (install speed), `devtools` (checks/build).
* Workflow: `{targets}` for pipelines; `testthat` for tests; `quarto` for docs, `make` for build.
* If missing, create: `docs/`, `docs/CHANGELOG.md`, `.Rbuildignore`, `.gitignore`, `.lintr`, `.Rproj` (if not present), and minimal CI scaffolding under `.github/workflows/` (do not enable secrets).

## 2) Hard Rules (enforced)

* **Do not run**: `Rscript`, `R CMD check`, `tar_make()`, `quarto render`, `make`, Docker/containers, or any command that mutates project state. Ask the user to run those; provide **copy‑paste** blocks.
* **Read‑only shell allowed**: You may run safe, read‑only shell commands solely for file inspection and navigation inside the repository working tree (e.g., `ls`, `rg`, `sed -n`, `head`, `tail`, `wc -l`, `stat`).
* Respect chunked reads (≤ 250 lines) and avoid streaming large contents.
* Prefer `rg` for searching; never open, stream or edit `renv.lock`.
* Never print or diff files that plausibly contain secrets (e.g., `.Renviron`).
* **Do not edit**: `.Rproj.user/` contents.
* For large files under `data/` or `data-raw/`, inspection is permitted via safe, read-only shell commands only (headers, schema, small samples, counts). Do not stream bulk contents. For deeper analysis, add a small R helper and ask the user to run it.
* **Never touch remotes**: no `git push`, no token usage, no network calls.
* **Secrets/data**: Treat `.Renviron`, `~/.R/Makevars`, tokens, and large/raw data as off-limits. Do not surface their contents. Place small example data under `inst/extdata/`.

## 3) Directory & File Conventions

* Package layout (if a package): `DESCRIPTION`, `NAMESPACE` (generated from roxygen), `CITATION.cff`, `R/`, `man/`, `tests/testthat/`, `inst/`, `vignettes/`, `data-raw/`, `docs/`, `reports/paper.qmd`, `outputs/`, `scripts/bootstrap.R`, `metadata/data_manifest.csv`, and `_targets.R` at repo root.
* Non-package project: still use `R/` for modular functions, `_targets.R` at root, `tests/` for unit tests, Quarto in `docs/` or `vignettes/`.
* Always add or update:

  * `docs/CHANGELOG.md` using **Keep a Changelog** style; one entry per meaningful change.
  * `docs/AGENT_NOTES.md` with a brief session log of edits (who/what/why).

## 4) Dependency Policy

* Prefer CRAN packages; record in `DESCRIPTION` (if a package) or a plain `dependencies.R` bootstrap (if not).
* Add roxygen `@importFrom` or fully qualify calls (e.g., `dplyr::mutate`).
* **Do not** snapshot `renv` yourself. When dependencies change, instruct the user to run:

```r
# In RStudio Console — Compendium (non-package)
renv::restore()
source("dependencies.R")
pak::pkg_install(project_dependencies())
renv::snapshot()

# Package projects
renv::restore()
pak::pak()                 # optional; resolves/installs fast
devtools::document()       # if roxygen headers changed
renv::snapshot()
```

## 5) Coding Style & Static Quality

* Style: use `styler` default; wrap ≤ 100 chars; snake_case for objects; S3/S4 consistent.
* Lint: ensure a project `.lintr` file; conform to `{lintr}` defaults.
* Errors via `rlang::abort()` with typed classes; use `cli::cli_*` for user-facing messages.
* Pure functions in `R/`; avoid `<<-`. Use `withr::local_*` for temporal state.
* **Explicit imports only** — Do **not** use `library()` or `require()` anywhere.
* Use `import::from("pkg", fun1, fun2)` in every function.
* Never call `setwd()`; always resolve paths with `here()`.
* Comment only non-obvious logic.
* Exported helpers require minimal Roxygen documentation.
* Always read datasets using `here()` for cross-system portability.
* Each function should be single-purpose; avoid code repetition.
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
* All summaries & artefacts must be derived from upstream targets; do not create side effects.

**Ask the user to run** (don’t run yourself):

```r
# Full pipeline
targets::tar_make()

# Specific targets
targets::tar_make(names = c("data_ingest", "model_fit"))

# Visualise DAG
targets::tar_visnetwork()
```

## 7) Testing & Checks

* Place tests in `tests/testthat/`; one file per exported function group.
* Write table-driven tests; include failure cases (`expect_error`, `expect_warning`).
* Coverage: include at least smoke + edge + error paths.
* Keep tests fast; avoid loading large data from `data/` or `data-raw/`. Use `inst/extdata/` samples.

**Ask the user to run (compendium)**:

```r
testthat::test_dir("tests/testthat", reporter = "summary")
# or
source("tests/run-tests.R")
```

**Ask the user to run (package)**:

```r
devtools::test()              # run tests
devtools::check()             # full package check
testthat::snapshot_review()   # if snapshot tests are used
```

## 8) Quarto / Documentation

* Place long-form docs in `docs/` or `vignettes/` (`.qmd`); keep runnable chunks (`eval=TRUE`) but assume user renders.
* Keep a `docs/DEVELOPMENT.qmd` with setup notes and decisions.

**Ask the user to run**:

```bash
# In a shell or RStudio Terminal
quarto render docs/
```

## 9) Git Workflow (local only)

* Commit frequently with Conventional Commits. Prefix automated commits with `agent:`.

  * Examples: `agent: feat(pipeline): add dynamic branching for site-level models`
* **Never** push, fetch, or change remotes.
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
2. Leave an inline `# TODO(JANUS):` with a short rationale and alternative considered.
3. Add a note in `docs/AGENT_NOTES.md`.
4. If the change would break public API, **stop and ask** for confirmation with a short diff and rollback plan.

## 15) Standard User Command Blocks (you provide, they run)

**Bootstrapping — Compendium (RStudio Console):**

```r
renv::restore(prompt = FALSE)
source("dependencies.R")
pak::pkg_install(project_dependencies())
testthat::test_dir("tests/testthat", reporter = "summary")
```

**Bootstrapping — Package:**

```r
renv::restore(prompt = FALSE)
pak::pak()                 # optional speed-up
devtools::document()
devtools::test()
```

**Pipeline:**

```r
targets::tar_make()
```

**Full package check:**

```r
devtools::check()
```

**Render docs:**

```bash
quarto render docs/
```

## 16) Success Criteria

* All tests pass locally for the user; `{targets}` completes cleanly on a fresh `renv::restore()`.
* Changelog updated; docs reflect pipeline and usage.
* No remote side-effects; repo remains buildable on Windows/macOS/Linux.
