#' Preview data files listed in the manifest
#'
#' For CSV files listed in the manifest, read up to `n_max` rows and return a
#' summary with row/column counts. Non-CSV files are skipped.
#'
#' @param manifest A data frame with columns `id`, `path`, and `abs_path`.
#' @param n_max Maximum number of rows to read per file for preview.
#' @return A tibble with columns `id`, `path`, `exists`, `n_rows`, `n_cols`.
#' @export
preview_manifest <- function(manifest, n_max = 100L) {
  fn_env <- environment()
  import::from("readr", read_csv, .into = fn_env)
  import::from("dplyr", tibble, mutate, select, bind_rows, .into = fn_env)
  import::from("tibble", tibble, .into = fn_env)
  import::from("cli", cli_warn, .into = fn_env)
  import::from("rlang", abort, .into = fn_env)

  if (!all(c("id", "path", "abs_path") %in% names(manifest))) {
    abort("`manifest` must contain id, path, abs_path columns.", class = "preview_manifest_bad_cols")
  }

  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    id <- manifest$id[[i]]
    rel <- manifest$path[[i]]
    p  <- manifest$abs_path[[i]]
    ex <- file.exists(p)
    if (!ex) {
      return(tibble::tibble(id = id, path = rel, exists = FALSE, n_rows = NA_integer_, n_cols = NA_integer_))
    }
    if (!grepl("\\.csv$", tolower(rel))) {
      return(tibble::tibble(id = id, path = rel, exists = TRUE, n_rows = NA_integer_, n_cols = NA_integer_))
    }
    dat <- try(readr::read_csv(p, n_max = n_max, show_col_types = FALSE, progress = FALSE), silent = TRUE)
    if (inherits(dat, "try-error")) {
      cli_warn(sprintf("Failed to read CSV for preview: %s", rel))
      return(tibble::tibble(id = id, path = rel, exists = TRUE, n_rows = NA_integer_, n_cols = NA_integer_))
    }
    tibble::tibble(id = id, path = rel, exists = TRUE, n_rows = nrow(dat), n_cols = ncol(dat))
  })

  dplyr::bind_rows(rows)
}
