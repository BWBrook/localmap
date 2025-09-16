#' Default Atlas occurrence fields
#'
#' Provides a reusable set of field names requested from the Atlas of Living
#' Australia when downloading occurrence records.
#'
#' @return Character vector of field names.
#' @export
ala_default_occurrence_fields <- function() {
  c(
    "decimalLatitude",
    "decimalLongitude",
    "eventDate",
    "vernacularName",
    "scientificName",
    "taxonConceptID",
    "dataResourceName"
  )
}
