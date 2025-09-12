## SLURM execution profile for {targets}
##
## Uses future.batchtools with a SLURM template. Configure workers/time/mem
## via config/config.yaml under the `slurm:` profile or environment variables.

future::plan(
  future.batchtools::batchtools_slurm,
  template = file.path("inst", "templates", "batchtools", "slurm.tmpl")
)

## Optionally respect config for default workers
cfg <- try(config::get(file = "config/config.yaml"), silent = TRUE)
if (!inherits(cfg, "try-error") && is.list(cfg) && !is.null(cfg$workers)) {
  options(mc.cores = as.integer(cfg$workers))
}
