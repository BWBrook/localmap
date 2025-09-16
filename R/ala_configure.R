#' Configure galah for Atlas of Living Australia access
#'
#' Establishes a galah session using values from the project configuration.
#'
#' @param settings Optional list with elements `email`, `atlas`, `caching`, and
#'   `verbose`. If `email` is missing or empty, the function looks for the
#'   `ALA_EMAIL` environment variable.
#' @return A list with the resolved configuration.
#' @export
ala_configure <- function(settings = NULL) {
  fn_env <- environment()
  import::from("checkmate", test_string, assert_integerish, .into = fn_env)
  import::from("galah", galah_config, .into = fn_env)
  import::from("rlang", abort, .into = fn_env)

  if (is.null(settings)) {
    settings <- list()
  } else if (!is.list(settings)) {
    settings <- as.list(settings)
  }

  email <- settings$email
  if (!test_string(email, min.chars = 1)) {
    email <- Sys.getenv("ALA_EMAIL", unset = "")
  }
  if (!test_string(email, min.chars = 1)) {
    abort(
      c(
        "An email address is required to authenticate with the ALA API.",
        i = "Set `cfg$ala$email` or the `ALA_EMAIL` environment variable."
      ),
      class = "ala_configure_missing_email"
    )
  }

  atlas <- settings$atlas
  if (!test_string(atlas, min.chars = 1)) {
    atlas <- "ALA"
  }

  caching <- settings$caching
  if (is.null(caching)) {
    caching <- TRUE
  } else {
    caching <- isTRUE(caching)
  }

  verbose <- settings$verbose
  if (is.null(verbose)) {
    verbose <- FALSE
  } else {
    verbose <- isTRUE(verbose)
  }

  reason <- settings$download_reason_id
  if (is.null(reason)) {
    reason <- 4L
  } else {
    assert_integerish(reason, lower = 1L, len = 1L, any.missing = FALSE)
    reason <- as.integer(reason)
  }

  galah_config(
    atlas = atlas,
    email = email,
    caching = caching,
    verbose = verbose,
    download_reason_id = reason
  )

  list(
    atlas = atlas,
    email = email,
    caching = caching,
    verbose = verbose,
    download_reason_id = reason
  )
}
