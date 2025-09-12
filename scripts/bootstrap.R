# Bootstrap script for local setup (run in RStudio Console)

message("Restoring renv and installing project dependencies...")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore(prompt = FALSE)
source("dependencies.R")
if (exists("project_dependencies")) {
  if (requireNamespace("pak", quietly = TRUE)) {
    pak::pkg_install(project_dependencies())
  } else {
    install.packages(project_dependencies())
  }
}
if (requireNamespace("devtools", quietly = TRUE)) devtools::document()
message("Done. You can now run tests and the pipeline.")

