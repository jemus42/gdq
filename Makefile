build:
	Rscript -e "pak::local_install(upgrade = FALSE)"

.PHONY: donations
donations:
	Rscript data-raw/data-gdqtracker.R
