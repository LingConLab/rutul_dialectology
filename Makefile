R_OPTS = --vanilla

.DEFAULT_GOAL: all
.PHONY: clean all

all: compile clean

compile:
	Rscript scripts/generate_qmd_and_compile.R
clean:
	rm -rf docs/data docs/html docs/tests docs/data docs/DESCRIPTION docs/LICENSE docs/Makefile docs/scripts *.html
test:
	R -q -e 'testthat::test_dir("tests")'
