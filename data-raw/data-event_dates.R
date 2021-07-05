# Proper event dates ----
# https://en.wikipedia.org/wiki/Games_Done_Quick#List_of_marathons

library(lubridate)
library(dplyr)

event_dates <- tribble(
  ~event,     ~start,                        ~end,
  # Awesome Games Done Quick
  "AGDQ2011", ymd("2011-01-06", tz = "UTC"), ymd("2011-01-11", tz = "UTC"),
  "AGDQ2012", ymd("2012-01-04", tz = "UTC"), ymd("2012-01-09", tz = "UTC"),
  "AGDQ2013", ymd("2013-01-06", tz = "UTC"), ymd("2013-01-12", tz = "UTC"),
  "AGDQ2014", ymd("2014-01-05", tz = "UTC"), ymd("2014-01-11", tz = "UTC"),
  "AGDQ2015", ymd("2015-01-04", tz = "UTC"), ymd("2015-01-10", tz = "UTC"),
  "AGDQ2016", ymd("2016-01-03", tz = "UTC"), ymd("2016-01-10", tz = "UTC"),
  "AGDQ2017", ymd("2017-01-08", tz = "UTC"), ymd("2017-01-15", tz = "UTC"),
  "AGDQ2018", ymd("2018-01-07", tz = "UTC"), ymd("2018-01-14", tz = "UTC"),
  "AGDQ2019", ymd("2019-01-06", tz = "UTC"), ymd("2019-01-12", tz = "UTC"),
  "AGDQ2020", ymd("2020-01-05", tz = "UTC"), ymd("2020-01-12", tz = "UTC"),
  "AGDQ2021", ymd("2021-01-03", tz = "UTC"), ymd("2021-01-10", tz = "UTC"),
  # Summer Games Done Quick
  "SGDQ2011", ymd("2011-08-04", tz = "UTC"), ymd("2011-08-06", tz = "UTC"),
  "SGDQ2012", ymd("2012-05-24", tz = "UTC"), ymd("2012-05-28", tz = "UTC"),
  "SGDQ2013", ymd("2013-07-25", tz = "UTC"), ymd("2013-07-30", tz = "UTC"),
  "SGDQ2014", ymd("2014-06-22", tz = "UTC"), ymd("2014-06-28", tz = "UTC"),
  "SGDQ2015", ymd("2015-07-26", tz = "UTC"), ymd("2015-08-02", tz = "UTC"),
  "SGDQ2016", ymd("2016-07-03", tz = "UTC"), ymd("2016-07-09", tz = "UTC"),
  "SGDQ2017", ymd("2017-07-02", tz = "UTC"), ymd("2017-07-09", tz = "UTC"),
  "SGDQ2018", ymd("2018-06-24", tz = "UTC"), ymd("2018-07-01", tz = "UTC"),
  "SGDQ2019", ymd("2019-06-23", tz = "UTC"), ymd("2019-06-30", tz = "UTC"),
  "SGDQ2020", ymd("2020-08-16", tz = "UTC"), ymd("2020-08-23", tz = "UTC"),
  "SGDQ2021", ymd("2021-07-04", tz = "UTC"), ymd("2021-07-11", tz = "UTC")
) %>%
  mutate(event_duration = start %--% end / ddays(1))

event_index <- gdqdonations::tracker_run_index()

event_index <- dplyr::left_join(event_dates, event_index, by = "event")

usethis::use_data(
  event_index, overwrite = TRUE,
  compress = "xz", version = 3
)

