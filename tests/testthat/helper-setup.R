testthat::local_edition(3)
set.seed(1)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(path = ".", helpers = FALSE, attach_testthat = FALSE)
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(path = ".", helpers = FALSE)
} else {
  stop(
    "pkgload or devtools is required to load the localmap package during tests.",
    call. = FALSE
  )
}
