#' Read project configuration
#'
#' Reads `config/config.yaml` using the active config profile (defaults to
#' `Sys.getenv("R_CONFIG_ACTIVE", "default")`). Returns a named list.
#'
#' @param profile Character scalar. Config profile name.
#' @return A named list with configuration values.
#' @export
cfg_read <- function(profile = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  import::from("here", here)
  import::from("config", get)
  import::from("rlang", abort)

  path <- here("config", "config.yaml")
  if (!file.exists(path)) {
    abort(
      c(
        "Configuration file not found.",
        i = sprintf("Expected at: %s", path)
      ),
      class = "cfg_read_missing_config"
    )
  }

  cfg <- get(file = path, config = profile)
  if (!is.list(cfg)) {
    abort("Config must resolve to a list.", class = "cfg_read_invalid_config")
  }
  cfg
}

