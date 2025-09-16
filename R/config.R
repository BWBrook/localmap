#' Read project configuration
#'
#' Reads `config/config.yaml` using the active config profile (defaults to
#' `Sys.getenv("R_CONFIG_ACTIVE", "default")`). Returns a named list.
#'
#' @param profile Character scalar. Config profile name.
#' @param config_path Optional path to the YAML file. Defaults to
#'   `here::here("config", "config.yaml")`.
#' @return A named list with configuration values.
#' @export
cfg_read <- function(profile = Sys.getenv("R_CONFIG_ACTIVE", "default"),
                     config_path = NULL) {
  fn_env <- environment()
  import::from("here", here, .into = fn_env)
  import::from("config", get, .into = fn_env)
  import::from("rlang", abort, .into = fn_env)

  if (is.null(config_path)) {
    config_path <- here("config", "config.yaml")
  }
  path <- config_path
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
