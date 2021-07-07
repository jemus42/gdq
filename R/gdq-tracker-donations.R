#' Get page count of GDQ tracker
#'
#' @param event `["latest"]` Event such as `"AGDQ2021"`, case insensitive. The default,
#' `"latest"`, is an alias for `event_index$event[nrow(event_index)]`.
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
    event <- gdqdonations::event_index$event[nrow(gdqdonations::event_index)]
  }

  event <- toupper(event)
  url <- gdqdonations::event_index$tracker_donation_url[gdqdonations::event_index$event == event]
  url <- paste0(gdq_base_url, url)

  rvest::read_html(url) %>%
    rvest::html_node("#page+ label") %>%
    rvest::html_text() %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()
}

#' Get a single page from the tracker
#'
#' @inheritParams get_page_count
#' @param page `[1]`: Page to get, a single number.
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' get_donation_page(event = "sgdq2021", page = 1)
#' }
get_donation_page <- function(event = "latest", page = 1) {

  if (event == "latest") {
    event <- gdqdonations::event_index$event[nrow(gdqdonations::event_index)]
  }

  event <- toupper(event)
  url <- gdqdonations::event_index$tracker_donation_url[gdqdonations::event_index$event == event]
  url <- paste0(gdq_base_url, url, "?page=", page)

  rvest::read_html(url) %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    purrr::set_names(c("name", "time", "amount", "comment")) %>%
    dplyr::mutate(
      time = lubridate::ymd_hms(.data$time),
      amount = stringr::str_remove(.data$amount, "\\$"),
      amount = stringr::str_remove(.data$amount, ","),
      amount = as.numeric(.data$amount)
    )
}

#' Get all donations for an event from the tracker
#'
#' @inheritParams get_page_count
#' @param delay `[0.5]`: Seconds to wait between pages. Don't annoy the webserver.
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' donations_sgdq2021 <- get_donations(event = "sgdq2021")
#' }
get_donations <- function(event = "latest", delay = .5) {

  if (event == "latest") {
    event <- gdqdonations::event_index$event[nrow(gdqdonations::event_index)]
  }

  event <- toupper(event)
  pages <- seq_len(get_page_count(event = event))

  prg <- cli::cli_progress_bar(
    name = glue::glue("Getting {toupper(event)}"),
    type = "iterator",
    # format = glue::glue("{event}: [:bar] :percent (:elapsedfull)"),
    total = length(pages),
    clear = TRUE,
    current = TRUE
  )

  purrr::map_df(pages, ~{
    Sys.sleep(delay)
    cli::cli_progress_update(id = prg)
    get_donation_page(event = event, page = .x)
  })
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
#' assemble_donations()
#' }
assemble_donations <- function(events = NULL, cache = TRUE) {

  if (is.null(events)) {
    events <- fs::dir_ls(
      fs::path(
        getOption("gdq_cache_dir"),
        "gamesdonequick.com"
      ),
      regexp = "donations_[as]gdq\\d+\\.rds"
    )
  }

  amount_breaks <- purrr::map(c(5, 10, 25), ~.x * 10^{0:5}) %>%
    purrr::flatten_dbl() %>%
    sort() %>%
    c(0, .)
  amount_c_labels <- paste0("<= ", scales::dollar(amount_breaks[-1]))

  all_donations <- purrr::map_df(events, ~{
    readRDS(.x) %>%
      dplyr::mutate(
        event = stringr::str_extract(.x, "[as]gdq\\d{4}") %>%
          stringr::str_to_upper()
      )
  }) %>%
    dplyr::arrange(.data$time) %>%
    dplyr::left_join(
      gdqdonations::event_index,
      by = "event"
    ) %>%
    dplyr::arrange(.data$time) %>%
    dplyr::mutate(
      day = lubridate::wday(.data$time, label = TRUE),
      day_num = paste0(.data$day, " (", lubridate::day(.data$time), ".)"),
      year = stringr::str_extract(.data$event, "\\d+"),
      gdq = stringr::str_remove(.data$event, "\\d+"),
      amount_c = cut(.data$amount, breaks = amount_breaks, labels = amount_c_labels),
      time_rel = ((.data$start %--% .data$time) / lubridate::dminutes(1)) /
        ((.data$start %--% .data$end)/lubridate::dminutes(1))
    ) %>%
    dplyr::select(-.data$start, -.data$end, -.data$event_duration)

  if (cache) {
    cli::cli_alert_info("Caching donation data at {.emph data/all_donations.rds}")
    saveRDS(all_donations, "data/all_donations.rds")
  }

  all_donations
}


#' Update donation data from GDQ tracker
#'
#' @param events Events such as `"agdq2019"`, case insensitive.
#' @param ignore_cache `[FALSE]`: If `TRUE`, ignore cached file and re-retrieve data.
#' @param in_progress `[FALSE]`: If `TRUE`, donations for in-progress events are retrieved.
#' @param sound `[TRUE]`: If `TRUE`, `beepr::beep(2)` is played after each event's
#' donations have been retrieved. Since scraping *will* take a long time for full
#' events, this seemed like a good idea.
#' @inheritParams get_donations
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
  events, delay = 0.5, ignore_cache = FALSE, in_progress = FALSE, sound = TRUE
  ) {
  prg <- cli::cli_progress_bar(name = "Getting donations", total = length(events))
  events <- toupper(events)

  donations <- purrr::walk(events, ~{
    cli::cli_progress_update(id = prg)
    cli::cli_alert_info("Current event: {toupper(.x)}")

    out_file <- fs::path(
      getOption("gdq_cache_dir"),
      "gamesdonequick.com",
      paste0("donations_", tolower(.x), ".rds")
    )

    if (!ignore_cache & file.exists(out_file)) return(tibble::tibble())

    if (!in_progress) {
      if (Sys.Date() < gdqdonations::event_index$end[gdqdonations::event_index$event == .x]) {
        return(tibble::tibble())
      }
    }

    usethis::use_directory(getOption("gdq_cache_dir"))
    get_donations(event = .x, delay = delay) %>%
      saveRDS(file = out_file)

    if (sound & requireNamespace("beepr", quietly = TRUE)) beepr::beep(2)
  })

  cli::cli_alert_success("Got donations!")
}
