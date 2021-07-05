library(gdqdonations)

events <- rev(tolower(gdqdonations::event_dates$event))

usethis::use_directory("data/gamesdonequick.com/donations/")
usethis::use_directory("data/gamesdonequick.com/runs/")


update_tracker_donations(event_index$event)
all_donations <- assemble_donations(cache = FALSE)
usethis::use_data(all_donations, overwrite = TRUE, compress = "xz", version = 3)

update_tracker_runs(event_index$event)
all_runs <- assemble_runs(cache = FALSE)
usethis::use_data(all_runs, overwrite = TRUE, compress = "xz", version = 3)
