build: doc
	Rscript -e "pak::local_install(upgrade = FALSE)"

.PHONY: donations
donations:
	Rscript data-raw/data-gdqtracker.R

README.md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd')"
	rm README.html

doc: README.md
	Rscript -e "devtools::document()"
