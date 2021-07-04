# Data acquisition
library(gdqdonations)
source("00_helpers-gdqvods.R")

events <- rev(tolower(gdqdonations::event_dates$event))

usethis::use_directory("data/gamesdonequick.com/donations/")
usethis::use_directory("data/gamesdonequick.com/runs/")
usethis::use_directory("data/gdqvods.com")


update_tracker_donations(events)
all_donations <- assemble_donations()

update_tracker_runs(events)
all_runs <- assemble_runs()

# Saving gdqvods runs ----
if (!file.exists("data/all_runs_gdqvods.rds")) {
  cli::cli_h1("Getting runs from gdqvods...")

  gdqvods <- get_gdqvods_runs(event_dates$event)
  saveRDS(gdqvods, "data/gdqvods.com/gdqvods_runs.rds")

  cli::cli_alert_success("Got & saved gdqvods runs!")
}

if (!file.exists("data/gdqvods.com/gdqvods_genres.rds")) {
  cli::cli_h1("Getting run genres from gdqvods...")

  genres <- get_gdqvods_by_genre()
  saveRDS(genres, "data/gdqvods.com/gdqvods_genres.rds")
}

if (!file.exists("data/gdqvods.com/gdqvods_categories.rds")) {
  cli::cli_h1("Getting run categories from gdqvods...")

  categories <- get_gdqvods_by_category()
  saveRDS(categories, "data/gdqvods.com/gdqvods_categories.rds")
}
