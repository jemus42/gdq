#' Get page count of GDQ tracker
#'
#' @param event `["latest"]` Event such as `"AGDQ2021"`, case insensitive. The default,
#' `"latest"`, is an alias for `[latest_event()]`.
#'
#' @return Page count as a `numeric(1)`.
#' @export
#'
#' @examples
#' \dontrun{
#' get_page_count(event = "latest")
#' }
get_page_count <- function(event = "latest") {
  if (event == "latest") {
    event <- latest_event()
  }

  event <- toupper(event)
  url <- event_index$tracker_donation_url[event_index$event == event]
  url <- paste0(gdq_base_url, url)

  rvest::read_html(url) |>
    rvest::html_node("#page+ label") |>
    rvest::html_text() |>
    stringr::str_extract("\\d+") |>
    as.numeric()
}

#' Get a single page from the tracker
#'
#' @inheritParams get_page_count
#' @param page `[1]`: Page to get, can also be a range of panges, e.g. `1:5`.
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' get_donation_page(event = "sgdq2011", page = 1:3)
#' }
get_donation_page <- function(event = "latest", page = 1) {
  if (event == "latest") {
    event <- latest_event()
  }

  event <- toupper(event)
  url <- event_index$tracker_donation_url[event_index$event == event]
  url = paste(gdq_base_url, url, sep = "/")
  session <- polite::bow(url, delay = 5)

  purrr::map(
    as.numeric(page),
    \(p) {
      polite::scrape(session, query = list(page = p)) |>
        rvest::html_table() |>
        purrr::pluck(1) |>
        purrr::set_names(c("name", "time", "amount", "comment")) |>
        dplyr::mutate(
          time = lubridate::ymd_hms(.data$time),
          amount = stringr::str_remove(.data$amount, "\\$") |>
            stringr::str_remove(",") |>
            as.numeric()
        )
    },
    .progress = list(
      name = event,
      format = "{cli::pb_name} {cli::pb_current}/{cli::pb_total} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = as.numeric(length(page))
    )
  ) |>
    purrr::list_rbind()
}

#' Get all donations for an event from the tracker
#'
#' @inheritParams get_page_count
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#'
#' donations_sgdq2025 <- get_donations(event = "sgdq2025")
#' }
get_donations <- function(event = "latest") {
  if (event == "latest") {
    event <- latest_event()
  }

  event <- toupper(event)
  pages <- seq_len(get_page_count(event = event))

  get_donation_page(event = event, page = pages)
}

#' Aggregate all donation data from the tracker in one file
#'
#' @param events `[NULL]`: Optional vector of file paths to individual `.rds` files.
#' @param cache `[TRUE]`: Save aggregated dataset at `"data/all_donations.rds"`.
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_donations(cache = TRUE)
#' }
assemble_donations <- function(events = NULL, cache = FALSE) {
  if (is.null(events)) {
    events <- fs::dir_ls(
      fs::path(
        getOption("gdq_cache_dir"),
        "gamesdonequick.com"
      ),
      regexp = "donations_[as]gdq\\d+\\.rds"
    )
  }

  all_donations <- purrr::map(
    events,
    \(x) {
      res <- readRDS(x)
      if (!inherits(res, "data.frame")) {
        res <- purrr::list_rbind(res)
      }

      res |>
        dplyr::mutate(
          event = stringr::str_extract(x, "[as]gdq\\d{4}") |>
            stringr::str_to_upper(),
          .before = "name"
        )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$time)

  if (cache) {
    cache_path <- fs::path(getOption("gdq_cache_dir"), "gdq_donations.rds")
    cli::cli_alert_info("Caching donation data at {.file {cache_path}}")
    saveRDS(all_donations, cache_path)
  }

  all_donations
}

#' Augment GDQ donation data with additional variables
#'
#' @param donations Donations `tibble` as returned by [`assemble_donations()`].
#'
#' @return A [`tibble`][tibble::tibble]
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_donations() |>
#'   augment_donations()
#' }
augment_donations <- function(donations) {
  amount_breaks <- c(
    0,
    as.integer(
      c(5, 10, 25) %o%
        10^{
          0:5
        }
    )
  )
  amount_c_labels <- paste0("<= ", scales::dollar(amount_breaks[-1]))

  donations |>
    dplyr::left_join(
      event_index |>
        dplyr::select("event", "start", "end") |>
        dplyr::left_join(
          summarize_runs(gdq_runs),
          by = "event"
        ),
      by = "event"
    ) |>
    dplyr::arrange(.data$time) |>
    dplyr::mutate(
      day = lubridate::wday(.data$time, label = TRUE),
      day_num = paste0(.data$day, " (", lubridate::day(.data$time), ".)"),
      year = stringr::str_extract(.data$event, "\\d+"),
      gdq = stringr::str_remove(.data$event, "\\d+"),
      amount_c = cut(
        .data$amount,
        breaks = amount_breaks,
        labels = amount_c_labels
      )
    ) |>
    # Dealing with time stuff is hard, this needs a better solution
    dplyr::mutate(
      start_guess = pmax(.data$start, .data$start_runs, na.rm = TRUE),
      end_guess = pmin(.data$end, .data$end_runs, na.rm = TRUE),
      time_rel = ((.data$start_guess %--% .data$time) /
        lubridate::dminutes(1)) /
        ((.data$start_guess %--% .data$end_guess) / lubridate::dminutes(1))
    ) |>
    dplyr::select(-dplyr::starts_with("start"), -dplyr::starts_with("end"))
}


#' Update donation data from GDQ tracker
#'
#' @param events Events such as `"agdq2019"`, case insensitive.
#' @param ignore_cache `[FALSE]`: If `TRUE`, ignore cached file and re-retrieve data.
#' @param in_progress `[FALSE]`: If `TRUE`, donations for in-progress events are retrieved.
#' @param sound `[TRUE]`: If `TRUE`, `beepr::beep(2)` is played after each event's
#' donations have been retrieved. Since scraping *will* take a long time for full
#' events, this seemed like a good idea.
#'
#' @return Invisibly: A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' update_tracker_donations(
#'   events = c("agdq2021", "sgdq2021"),
#'   ignore_cache = TRUE,
#'   in_progress = TRUE
#' )
#' }
update_tracker_donations <- function(
  events,
  ignore_cache = FALSE,
  in_progress = FALSE,
  sound = TRUE
) {
  prg <- cli::cli_progress_bar(
    name = "Getting donations",
    total = length(events)
  )
  events <- toupper(events)

  donations <- purrr::walk(
    events,
    \(x) {
      cli::cli_progress_update(id = prg)
      cli::cli_alert_info("Current event: {toupper(x)}")

      out_file <- fs::path(
        getOption("gdq_cache_dir"),
        "gamesdonequick.com",
        paste0("donations_", tolower(x), ".rds")
      )

      if (!ignore_cache & file.exists(out_file)) {
        return(tibble::tibble())
      }

      if (!in_progress) {
        if (Sys.Date() < event_index$end[event_index$event == x]) {
          return(tibble::tibble())
        }
      }

      usethis::use_directory(getOption("gdq_cache_dir"))
      get_donations(event = x) |>
        saveRDS(file = out_file)

      if (sound & requireNamespace("beepr", quietly = TRUE)) beepr::beep(2)
    }
  )

  cli::cli_alert_success("Got donations!")
}
