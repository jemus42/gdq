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

  runlinks <- rvest::read_html(glue::glue("{gdq_base_url}/tracker/runs/")) |>
    rvest::html_nodes(".list-group-item")

  tibble::tibble(
    event_name = rvest::html_text(runlinks),
    tracker_run_url = rvest::html_attr(runlinks, name = "href"),
    # Infer donations URL from runs URL, assuming same slug
    tracker_donation_url = rvest::html_attr(runlinks, name = "href") |>
      stringr::str_replace_all("runs", "donations"),
    event_slug = stringr::str_extract(.data$tracker_run_url, "[a-zA-Z0-9]+$"),
    event = stringr::str_to_upper(.data$event_slug)
  )
}
