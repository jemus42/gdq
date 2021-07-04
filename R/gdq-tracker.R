gdq_base_url <- "https://gamesdonequick.com"

#' Get GDQ tracker event overview
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' tracker_run_index()
#' }
tracker_run_index <- function() {

  runlinks <- rvest::read_html("https://gamesdonequick.com/tracker/runs/") %>%
    rvest::html_nodes(".list-group-item")

  donationlinks <- rvest::read_html("https://gamesdonequick.com/tracker/events/") %>%
    rvest::html_nodes(".list-group-item")

  donationlinks <- donationlinks[-1]

  index <- tibble::tibble(
    event_name = rvest::html_text(runlinks),
    tracker_run_url = rvest::html_attr(runlinks, name = "href"),
    tracker_donation_url = rvest::html_attr(donationlinks, name = "href") %>%
      stringr::str_replace_all("event", "donations"),
    event_slug = stringr::str_extract(tracker_run_url, "[a-zA-Z0-9]+$"),
    event = stringr::str_to_upper(event_slug)
  )

}
