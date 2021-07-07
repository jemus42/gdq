library(gdqdonations)

usethis::use_directory("data-cache/gamesdonequick.com/")

# Make sure previous events are all there
update_tracker_donations(event_index$event)

# Update in-progress event
update_tracker_donations(
  event_index$event[nrow(event_index)],
  ignore_cache = TRUE, in_progress = TRUE,
  sound = FALSE
)

# Assemble and save
gdq_donations <- assemble_donations(cache = FALSE)
usethis::use_data(gdq_donations, overwrite = TRUE, compress = "xz", version = 3)

update_tracker_runs(event_index$event)
gdq_runs <- assemble_runs(cache = FALSE)
usethis::use_data(gdq_runs, overwrite = TRUE, compress = "xz", version = 3)
