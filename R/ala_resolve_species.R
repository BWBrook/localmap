#' Resolve species queries against the Atlas of Living Australia
#'
#' Uses `galah::search_taxa()` (by default) to resolve the species requested in
#' `read_species_requests()`. Resolution prefers scientific-name matches when
#' supplied, otherwise falls back to common-name matches, then the first returned
#' candidate.
#'
#' @param species_requests Tibble of species queries as returned by
#'   [read_species_requests()].
#' @param search_fun Function used to perform the lookup. Defaults to
#'   `galah::search_taxa`. Primarily injected for testing.
#' @return A tibble with columns describing the original query and the resolved
#'   species metadata.
#' @export
ala_resolve_species <- function(species_requests, search_fun = galah::search_taxa) {
  fn_env <- environment()
  import::from("checkmate", assert_data_frame, assert_names, .into = fn_env)
  import::from("cli", cli_warn, .into = fn_env)
  import::from("dplyr", filter, mutate, pull, slice_head, .into = fn_env)
  import::from("purrr", map_dfr, .into = fn_env)
  import::from("rlang", .data, abort, is_function, .into = fn_env)
  import::from("stringr", str_to_lower, .into = fn_env)
  import::from("tibble", as_tibble, tibble, .into = fn_env)

  assert_data_frame(species_requests, min.rows = 1L)
  assert_names(colnames(species_requests), must.include = c("species_id", "common_name", "scientific_name"))
  if (!is_function(search_fun)) {
    abort("`search_fun` must be a function.", class = "ala_resolve_species_bad_search")
  }

  normalise_string <- function(x) {
    str_to_lower(trimws(x))
  }

  pick_column <- function(tbl, options) {
    cols <- intersect(options, colnames(tbl))
    if (length(cols) == 0L) {
      return(NA_character_)
    }
    cols[[1]]
  }

  resolve_one <- function(row) {
    query_terms <- unique(na.omit(c(row$scientific_name, row$common_name)))
    candidates <- map_dfr(query_terms, function(term) {
      result <- search_fun(term)
      if (is.null(result)) {
        return(tibble())
      }
      candidate <- as_tibble(result)
      if (nrow(candidate) == 0L) {
        return(tibble())
      }
      mutate(candidate, .search_term = term)
    })

    if (nrow(candidates) == 0L) {
      cli_warn("No taxonomic match found for {row$species_id}.")
      return(tibble(
        species_id = row$species_id,
        query_common_name = row$common_name,
        query_scientific_name = row$scientific_name,
        matched_scientific_name = NA_character_,
        matched_common_name = NA_character_,
        taxon_concept_id = NA_character_,
        search_term = NA_character_,
        match_type = "not_found"
      ))
    }

    rank_col <- pick_column(candidates, c("rank", "taxonRank"))
    candidates_raw <- candidates
    if (!is.na(rank_col)) {
      candidates <- filter(candidates, str_to_lower(.data[[rank_col]]) == "species")
      if (nrow(candidates) == 0L) {
        candidates <- candidates_raw
      }
    }

    sci_col <- pick_column(candidates, c("scientificName", "scientific_name", "name"))
    com_col <- pick_column(candidates, c("vernacularName", "common_name"))
    id_col <- pick_column(candidates, c("taxonConceptID", "guid", "taxon_concept_id"))

    match_type <- "fallback"
    selection <- candidates

    if (!is.na(row$scientific_name) && !is.na(sci_col)) {
      selection <- filter(
        candidates,
        normalise_string(.data[[sci_col]]) == normalise_string(row$scientific_name)
      )
      if (nrow(selection) > 0L) {
        match_type <- "scientific_name"
      }
    }

    if (nrow(selection) == 0L && !is.na(row$common_name) && !is.na(com_col)) {
      selection <- filter(
        candidates,
        normalise_string(.data[[com_col]]) == normalise_string(row$common_name)
      )
      if (nrow(selection) > 0L) {
        match_type <- "common_name"
      }
    }

    if (nrow(selection) == 0L) {
      selection <- candidates
    }

    chosen <- slice_head(selection, n = 1L)
    tibble(
      species_id = row$species_id,
      query_common_name = row$common_name,
      query_scientific_name = row$scientific_name,
      matched_scientific_name = if (!is.na(sci_col)) chosen[[sci_col]][[1]] else NA_character_,
      matched_common_name = if (!is.na(com_col)) chosen[[com_col]][[1]] else NA_character_,
      taxon_concept_id = if (!is.na(id_col)) chosen[[id_col]][[1]] else NA_character_,
      search_term = dplyr::pull(chosen, .search_term)[1],
      match_type = match_type
    )
  }

  map_dfr(seq_len(nrow(species_requests)), function(idx) {
    resolve_one(species_requests[idx, , drop = FALSE])
  })
}
