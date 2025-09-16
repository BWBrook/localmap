## localmap {targets} pipeline

targets::tar_option_set(
  seed = 1L,
  packages = character(0) # we use explicit namespace qualifiers/import::from within functions
)

# Source helpers
targets::tar_source("R")

list(
  # Configuration
  targets::tar_target(
    cfg,
    cfg_read(),
    description = "Project configuration (paths, compute flags)."
  ),

  # Manifest of input files
  targets::tar_target(
    raw_manifest,
    read_manifest(cfg$data$manifest),
    description = "CSV manifest of input files."
  ),

  # Track file dependencies declared in the manifest (vector of paths)
  targets::tar_target(
    input_files,
    raw_manifest$abs_path,
    format = "file",
    description = "File paths declared in the manifest (tracked as file deps)."
  ),

  # Lightweight preview of input CSVs (row/column counts)
  targets::tar_target(
    data_preview,
    preview_manifest(raw_manifest, n_max = 100L),
    description = "Per-file preview (n_rows, n_cols) for CSV inputs."
  ),

  # Combine CSV inputs when columns align across files
  targets::tar_target(
    combined_data,
    combine_manifest_csvs(raw_manifest),
    description = "Combined tibble of CSV inputs using common columns."
  ),

  # Example summary derived from the manifest
  targets::tar_target(
    manifest_summary,
    {
      dplyr::summarise(
        raw_manifest,
        n_rows = dplyr::n(),
        n_files = dplyr::n_distinct(abs_path)
      )
    },
    description = "Simple summary of the manifest contents."
  ),

  # Report rendering: render to same directory as source file
  targets::tar_target(
    report,
    {
      qmd <- "reports/paper.qmd"
      out_dir <- dirname(qmd)
      # Render with Quarto if available; otherwise write a simple placeholder HTML
      has_quarto <- requireNamespace("quarto", quietly = TRUE) && !is.na(tryCatch(quarto::quarto_path(), error = function(e) NA))
      base <- tools::file_path_sans_ext(basename(qmd))
      html <- file.path(out_dir, paste0(base, ".html"))
      ok <- FALSE
      if (has_quarto) {
        ok <- isTRUE(tryCatch({
          quarto::quarto_render(input = qmd, quiet = TRUE)
          file.exists(html)
        }, error = function(e) FALSE))
      }
      if (!ok) {
        writeLines("<html><body><p>Report placeholder (Quarto not available or render failed).</p></body></html>", html)
      }
      html
    },
    format = "file",
    description = "Rendered Quarto report artifact."
  )
)
