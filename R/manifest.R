#' Read a CSV manifest of input files
#'
#' Expects a CSV with columns `id` and `path`. Returns a tibble with an
#' additional `abs_path` column resolved relative to the project root.
#'
#' @param manifest_path Character path to the CSV manifest.
#' @return A tibble with columns `id`, `path`, and `abs_path`.
#' @export
read_manifest <- function(manifest_path) {
  import::from("here", here)
  import::from("readr", read_csv)
  import::from("dplyr", mutate)
  import::from("tibble", tibble)
  import::from("stringr", str_trim)
  import::from("checkmate", assert_subset, assert_names, any_string, assert_file_exists)
  import::from("cli", cli_warn, cli_inform)
  import::from("rlang", abort)

  if (!is.character(manifest_path) || length(manifest_path) != 1L) {
    abort("`manifest_path` must be a single character string.", class = "read_manifest_bad_path")
  }

  # Allow relative paths
  csv_path <- manifest_path
  if (!file.exists(csv_path)) {
    # Try resolving under repo root
    csv_path <- here(manifest_path)
  }
  if (!file.exists(csv_path)) {
    abort(
      c(
        "Manifest file not found.",
        i = sprintf("Tried: %s and %s", manifest_path, csv_path)
      ),
      class = "read_manifest_missing"
    )
  }

  dat <- read_csv(csv_path, show_col_types = FALSE, progress = FALSE)
  # Validate minimal schema
  needed <- c("id", "path")
  assert_names(colnames(dat), must.include = needed)
  dat$id <- str_trim(dat$id)
  dat$path <- str_trim(dat$path)

  out <- mutate(dat, abs_path = here(path))
  # Warn for missing files but do not abort; downstream targets may generate them
  missing <- out[!file.exists(out$abs_path), , drop = FALSE]
  if (nrow(missing) > 0L) {
    cli_warn(sprintf("%d manifest file(s) do not exist yet.", nrow(missing)))
  } else {
    cli_inform("All manifest files exist.")
  }
  out
}
