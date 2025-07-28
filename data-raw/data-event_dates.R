# Proper event dates ----
# https://en.wikipedia.org/wiki/Games_Done_Quick#List_of_marathons

library(lubridate)
library(dplyr)

# fmt: skip
event_dates <- tibble::tribble(
  ~event,     ~start,                        ~end,
  # Awesome Games Done Quick
  "AGDQ2011", ymd("2011-01-06", tz = "UTC"), ymd("2011-01-11", tz = "UTC"),
  "AGDQ2012", ymd_hms("2012-01-04 22:00:00", tz = "UTC"), ymd_hms("2012-01-07 17:15:00", tz = "UTC"),
  "AGDQ2013", ymd_hms("2013-01-06 18:15:00", tz = "UTC"), ymd_hms("2013-01-13 03:53:46", tz = "UTC"),
  "AGDQ2014", ymd_hms("2014-01-05 17:00:00", tz = "UTC"), ymd_hms("2014-01-12 09:17:17", tz = "UTC"),
  "AGDQ2015", ymd_hms("2015-01-04 17:00:00", tz = "UTC"), ymd_hms("2015-01-11 11:03:00", tz = "UTC"),
  "AGDQ2016", ymd_hms("2016-01-03 16:30:00", tz = "UTC"), ymd_hms("2016-01-10 08:02:14", tz = "UTC"),
  "AGDQ2017", ymd_hms("2017-01-08 16:30:00", tz = "UTC"), ymd_hms("2017-01-15 07:07:12", tz = "UTC"),
  "AGDQ2018", ymd_hms("2018-01-07 16:30:00", tz = "UTC"), ymd_hms("2018-01-14 09:54:07", tz = "UTC"),
  "AGDQ2019", ymd_hms("2019-01-06 16:30:00", tz = "UTC"), ymd_hms("2019-01-13 06:16:02", tz = "UTC"),
  "AGDQ2020", ymd_hms("2020-01-05 16:30:00", tz = "UTC"), ymd_hms("2020-01-12 07:51:00", tz = "UTC"),
  "AGDQ2021", ymd_hms("2021-01-03 16:30:00", tz = "UTC"), ymd_hms("2021-01-10 09:52:00", tz = "UTC"),
  "AGDQ2022", ymd_hms("2022-01-09 12:00:00", tz = "UTC"), ymd_hms("2022-01-16 12:00:00", tz = "UTC"),
  "AGDQ2023", ymd_hms("2023-01-08 12:00:00", tz = "UTC"), ymd_hms("2023-01-15 12:00:00", tz = "UTC"),
  "AGDQ2024", ymd_hms("2024-01-14 12:00:00", tz = "UTC"), ymd_hms("2024-01-21 12:00:00", tz = "UTC"),
  "AGDQ2025", ymd_hms("2025-01-05 12:00:00", tz = "UTC"), ymd_hms("2025-01-12 12:00:00", tz = "UTC"),

  # Summer Games Done Quick
  "SGDQ2011", ymd_hms("2011-08-04 19:00:00", tz = "UTC"), ymd_hms("2011-08-06 23:07:31", tz = "UTC"),
  "SGDQ2012", ymd_hms("2012-05-24 21:00:00", tz = "UTC"), ymd_hms("2012-05-28 18:13:00", tz = "UTC"),
  "SGDQ2013", ymd_hms("2013-07-25 18:00:00", tz = "UTC"), ymd_hms("2013-07-30 06:48:00", tz = "UTC"),
  "SGDQ2014", ymd_hms("2014-06-22 18:00:00", tz = "UTC"), ymd_hms("2014-06-29 13:02:00", tz = "UTC"),
  "SGDQ2015", ymd_hms("2015-07-26 16:00:00", tz = "UTC"), ymd_hms("2015-08-02 06:38:04", tz = "UTC"),
  "SGDQ2016", ymd_hms("2016-07-03 16:30:00", tz = "UTC"), ymd_hms("2016-07-10 08:00:00", tz = "UTC"),
  "SGDQ2017", ymd_hms("2017-07-02 16:30:00", tz = "UTC"), ymd_hms("2017-07-09 09:29:12", tz = "UTC"),
  "SGDQ2018", ymd_hms("2018-06-24 16:30:00", tz = "UTC"), ymd_hms("2018-07-01 07:50:54", tz = "UTC"),
  "SGDQ2019", ymd_hms("2019-06-23 16:30:00", tz = "UTC"), ymd_hms("2019-06-30 06:55:00", tz = "UTC"),
  "SGDQ2020", ymd_hms("2020-08-16 15:30:00", tz = "UTC"), ymd_hms("2020-08-23 07:42:00", tz = "UTC"),
  "SGDQ2021", ymd_hms("2021-07-04 15:30:00", tz = "UTC"), ymd_hms("2021-07-11 07:46:00", tz = "UTC"),
  "SGDQ2022", ymd_hms("2022-06-26 15:30:00", tz = "UTC"), ymd_hms("2022-07-03 16:00:00", tz = "UTC"),
  "SGDQ2023", ymd_hms("2023-05-28 12:00:00", tz = "UTC"), ymd_hms("2023-06-04 16:00:00", tz = "UTC"),
  "SGDQ2024", ymd_hms("2024-06-30 12:00:00", tz = "UTC"), ymd_hms("2024-07-07 16:00:00", tz = "UTC"),
  "SGDQ2025", ymd_hms("2025-07-06 12:00:00", tz = "UTC"), ymd_hms("2025-07-13 16:00:00", tz = "UTC")

) |>
  mutate(event_duration = start %--% end / ddays(1))

event_index <- gdq::tracker_run_index()

event_index <- dplyr::left_join(event_dates, event_index, by = "event") |>
  dplyr::arrange(.data$start)

usethis::use_data(
  event_index,
  overwrite = TRUE,
  compress = "xz",
  version = 3
)
