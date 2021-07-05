library(gdqdonations)

usethis::use_directory("data/gamesdonequick.com/donations/")
usethis::use_directory("data/gamesdonequick.com/runs/")


# Make sure previous events are all there
update_tracker_donations(event_index$event)

# Update in-progress event
update_tracker_donations(
  event_index$event[nrow(event_index)],
  ignore_cache = TRUE, in_progress = TRUE
)

# Assemble and save
all_donations <- assemble_donations(cache = FALSE)
usethis::use_data(all_donations, overwrite = TRUE, compress = "xz", version = 3)

update_tracker_runs(event_index$event)
all_runs <- assemble_runs(cache = FALSE)
usethis::use_data(all_runs, overwrite = TRUE, compress = "xz", version = 3)
