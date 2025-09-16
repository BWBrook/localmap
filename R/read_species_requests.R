#' Read species request definitions
#'
#' Reads a CSV file describing the species to query from the Atlas of Living
#' Australia. Each row should supply at least one of `common_name` or
#' `scientific_name`. A `species_id` column is optional; if missing, unique ids
#' are generated from the row numbers.
#'
#' @param path Character scalar. Path to the CSV file, relative to the project
#'   root or absolute.
#' @return A tibble with columns `species_id`, `common_name`, and
#'   `scientific_name`.
#' @export
read_species_requests <- function(path) {
  fn_env <- environment()
  import::from("checkmate", assert_character, .into = fn_env)
  import::from("dplyr", if_else, mutate, row_number, select, .into = fn_env)
  import::from("here", here, .into = fn_env)
  import::from("readr", read_csv, .into = fn_env)
  import::from("rlang", abort, .into = fn_env)
  import::from("stringr", str_trim, .into = fn_env)
  import::from("tibble", as_tibble, .into = fn_env)

  assert_character(path, any.missing = FALSE, len = 1L)

  candidate_paths <- c(path, here(path))
  path_resolved <- candidate_paths[file.exists(candidate_paths)][1]
  if (is.na(path_resolved)) {
    abort(
      c(
        "Species request file not found.",
        i = sprintf("Tried: %s", paste(candidate_paths, collapse = ", "))
      ),
      class = "read_species_requests_missing"
    )
  }

  raw <- read_csv(path_resolved, show_col_types = FALSE, progress = FALSE)
  required <- c("common_name", "scientific_name")
  missing_cols <- setdiff(required, colnames(raw))
  if (length(missing_cols) > 0L) {
    abort(
      c(
        "Species request file is missing required columns.",
        i = sprintf("Missing: %s", paste(missing_cols, collapse = ", "))
      ),
      class = "read_species_requests_schema"
    )
  }

  has_id <- "species_id" %in% colnames(raw)

  out <- mutate(
    raw,
    species_id = if (has_id) species_id else sprintf("species_%02d", row_number())
  )

  if (any(is.na(out$species_id) | out$species_id == "")) {
    abort("`species_id` values must be non-missing.", class = "read_species_requests_id_missing")
  }
  if (any(duplicated(out$species_id))) {
    abort("`species_id` values must be unique.", class = "read_species_requests_id_duplicate")
  }

  cleaned <- mutate(
    out,
    common_name = str_trim(common_name),
    scientific_name = str_trim(scientific_name),
    common_name = if_else(common_name == "", NA_character_, common_name),
    scientific_name = if_else(scientific_name == "", NA_character_, scientific_name)
  )

  invalid_rows <- which(is.na(cleaned$common_name) & is.na(cleaned$scientific_name))
  if (length(invalid_rows) > 0L) {
    abort(
      c(
        "Each species request must include a common or scientific name.",
        i = sprintf("Invalid rows: %s", paste(invalid_rows, collapse = ", "))
      ),
      class = "read_species_requests_no_query"
    )
  }

  select(cleaned, species_id, common_name, scientific_name) |> as_tibble()
}
