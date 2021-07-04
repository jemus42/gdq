# Data acquisition
library(gdqdonations)
source("00_helpers-gdqvods.R")

events <- rev(tolower(gdqdonations::event_dates$event))

usethis::use_directory("data/gamesdonequick.com/donations/")
usethis::use_directory("data/gamesdonequick.com/runs/")
usethis::use_directory("data/gdqvods.com")


# Donations ----
# prg <- cli_progress_bar(total = length(events))
cli_h1("Getting donations...")

purrr::walk(events, ~{
  # prg$tick()

  cli::cli_alert_info("Current event: {toupper(.x)}")

  out_file <- file.path("data/gamesdonequick.com/donations/", paste0("donations_", .x, ".rds"))

  if (file.exists(out_file) &
      Sys.Date() > event_dates$end[tolower(event_dates$event) == .x]) {
    return(tibble())
  }

  dntns <- get_donations(event = .x)
  saveRDS(object = dntns, file = out_file)

  beepr::beep(2)
})
cli::cli_alert_success("Got donations!")

# Cache assembled donations dataset
all_donations <- assemble_donations()

# Runs (GDQ) ----
prg <- cli::cli_progress_bar(total = length(events))
cli::cli_h1("Getting runs from GDQ tracker...")

walk(events, ~{
  prg$tick()
  cli::cli_text("Current event: {toupper(.x)}")
  out_file <- paste0("data/gamesdonequick.com/runs/runs_", .x, ".rds")

  if (file.exists(out_file) | .x %in% c("agdq2011", "agdq2021")) {
    return(tibble())
  }

  get_runs(event = .x) %>%
    saveRDS(out_file)
})

cli::cli_alert_success("Got runs from GDQ tracker!")

# Cache assembled runs dataset
all_runs <- assemble_runs()
saveRDS(all_runs, "data/all_runs_gdqtracker.rds")

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
