build:
	Rscript -e "pak::local_install(upgrade = FALSE)"

.PHONY: donations
donations:
	Rscript data-raw/data-gdqtracker.R

README.md:
	Rscript -e "rmarkdown::render('README.Rmd')"
	rm README.html
