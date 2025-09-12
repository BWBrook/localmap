.PHONY: help bootstrap test pipeline docs check

help:
	@echo "Common tasks:"
	@echo "  make bootstrap   # restore renv and install deps"
	@echo "  make test        # run unit tests"
	@echo "  make pipeline    # run targets pipeline"
	@echo "  make docs        # render docs/ with Quarto"
	@echo "  make check       # run devtools::check()"

bootstrap:
	@Rscript -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv'); renv::restore(prompt=FALSE)" \
	        -e "source('dependencies.R'); if (exists('project_dependencies')) if (requireNamespace('pak', quietly=TRUE)) pak::pkg_install(project_dependencies()) else install.packages(project_dependencies())" \
	        -e "if (requireNamespace('devtools', quietly=TRUE)) devtools::document()"

test:
	@Rscript -e "testthat::test_dir('tests/testthat', reporter='summary')"

pipeline:
	@Rscript -e "targets::tar_make()"

docs:
	@quarto render docs/

check:
	@Rscript -e "if (requireNamespace('devtools', quietly=TRUE)) devtools::check()"

site:
	@Rscript -e "if (requireNamespace('pkgdown', quietly=TRUE)) pkgdown::build_site(preview = FALSE) else install.packages('pkgdown'); pkgdown::build_site(preview = FALSE)"
