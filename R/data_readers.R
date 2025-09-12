#' Combine CSV files from a manifest when columns align
#'
#' Reads all existing `.csv` files referenced in a manifest and returns a
#' single tibble consisting of the intersection of common columns across files.
#' Non-CSV or missing files are skipped with a warning.
#'
#' @param manifest A tibble/data.frame containing `path` and `abs_path`.
#' @return A tibble with the common columns across all readable CSV files.
#' @export
combine_manifest_csvs <- function(manifest) {
  import::from("readr", read_csv)
  import::from("purrr", map, keep, compact)
  import::from("dplyr", bind_rows, select, intersect)
  import::from("cli", cli_warn)
  import::from("rlang", abort)

  if (!all(c("path", "abs_path") %in% names(manifest))) {
    abort("`manifest` must contain path and abs_path columns.", class = "combine_manifest_bad_cols")
  }

  idx <- grepl("\\.csv$", tolower(manifest$path)) & file.exists(manifest$abs_path)
  paths <- manifest$abs_path[idx]
  if (length(paths) == 0L) {
    return(tibble::tibble())
  }

  dfs <- purrr::map(paths, function(p) {
    dat <- try(readr::read_csv(p, show_col_types = FALSE, progress = FALSE), silent = TRUE)
    if (inherits(dat, "try-error")) {
      cli::cli_warn(sprintf("Failed to read CSV: %s", basename(p)))
      return(NULL)
    }
    dat
  }) |> purrr::compact()

  if (length(dfs) == 0L) {
    return(tibble::tibble())
  }

  # Determine intersection of column names
  common <- Reduce(base::intersect, lapply(dfs, names))
  if (length(common) == 0L) {
    cli::cli_warn("No common columns across CSV inputs; returning empty tibble.")
    return(tibble::tibble())
  }

  dplyr::bind_rows(lapply(dfs, function(x) dplyr::select(x, dplyr::all_of(common))))
}
