gdq_base_url <- "https://tracker.gamesdonequick.com"

# Guess I need to keep track of that number now.
# Last checked 2025-07-27
.usd_to_eur <- 0.85

# To be removed over time as code is made more... gooder.
globalVariables(c(
  ".",
  "end_runs",
  "event_index",
  "gdq_runs",
  "run_duration",
  "run_end",
  "run_start",
  "start_runs"
))
