R := R_LIBS_USER=$(CURDIR)/.R/library Rscript

.PHONY: all deps run replicate core audit report paper test lint format clean

all: run test lint

run: replicate audit report

replicate:
	$(R) src/prepare.R
	$(R) src/replicate.R
	$(R) src/compare_published.R

core: replicate
	$(R) src/paper_figures.R

audit:
	$(R) src/audit_inference.R
	$(R) src/audit_functional_form.R
	$(R) src/responder_quality.R
	$(R) src/heterogeneity.R
	$(R) src/headline_numbers.R
	$(R) src/contact_sensitivity.R

report:
	$(R) src/paper_figures.R
	$(R) src/figures.R
	$(R) -e 'knitr::knit("README.Rmd", output = "README.md", quiet = TRUE)'
	$(R) -e 'rmarkdown::render("ms/paper.Rmd", quiet = TRUE)'

paper: run

test:
	$(R) tests/run_tests.R

lint:
	$(R) -e 'x <- lintr::lint_dir("src"); y <- lintr::lint_dir("tests"); print(c(x, y)); stopifnot(length(x) + length(y) == 0L)'

format:
	$(R) -e 'styler::cache_deactivate(); styler::style_dir("src"); styler::style_dir("tests")'

deps:
	@mkdir -p .R/library
	$(R) -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org"); renv::restore(library = ".R/library", prompt = FALSE)'

clean:
	rm -f output/* figs/* data/derived/*
	rm -f ms/paper.pdf ms/paper.tex
