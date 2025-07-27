library(gdqdonations)

usethis::use_directory("data-cache/gamesdonequick.com/")

# Donations (via gamesdonequick.com) -------------------------------------------------------------------------------

# Make sure previous events are all there
update_tracker_donations(event_index$event)

# Update in-progress event
# update_tracker_donations(
#   latest_event(),
#   ignore_cache = TRUE,
#   in_progress = TRUE,
#   sound = FALSE
# )

# Assemble and save
gdq_donations <- assemble_donations(cache = TRUE)
usethis::use_data(gdq_donations, overwrite = TRUE)

# Runs (via gamesdonequick.com) -----------------------------------------------------------------------------------
update_tracker_runs(latest_event())

update_tracker_runs(
  event_index$event[-1],
  in_progress = TRUE,
  ignore_cache = TRUE
)
gdq_runs <- assemble_runs(cache = TRUE)
usethis::use_data(gdq_runs, overwrite = TRUE)
