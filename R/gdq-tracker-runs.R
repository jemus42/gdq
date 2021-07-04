
#' Title
#'
#' @inheritParams get_page_count
#'
#' @return
#' @export
#'
#' @examples
get_runs <- function(event) {
  rvest::read_html(paste0("https://gamesdonequick.com/tracker/runs/", event)) %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    purrr::set_names(c("run", "players", "description", "run_start", "run_end", "bidwars")) %>%
    dplyr::mutate(
      run_start = lubridate::ymd_hms(run_start),
      run_end = lubridate::ymd_hms(run_end),
      run_duration_s = as.numeric(difftime(run_end, run_start, units = "secs")),
      run_duration_hms = hms::hms(seconds = run_duration_s),
      event = stringr::str_to_upper(str_extract(event, "[as]gdq\\d+")),
      year = stringr::str_extract(event, "\\d+"),
      gdq = stringr::str_remove(event, "\\d+")
    ) %>%
    dplyr::arrange(run_start) %>%
    as_tibble()
}

#' Update all runs from GDQ tracker
#'
#' @inheritParams assemble_donations
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_runs(cache = FALSE)
#' }
assemble_runs <- function(events = NULL, cache = TRUE) {

  if (is.null(events)) {
    events <- fs::dir_ls(
      "data/gamesdonequick.com/runs/",
      regexp = "runs_[as]gdq\\d+\\.rds"
    )
  }

  runs <- purrr::map_df(events, readRDS) %>%
    dplyr::arrange(run_start) %>%
    as_tibble()

  if (cache) {
    cli::cli_alert_info("Caching run data at {.emph data/all_runs_gdqtracker.rds}")
    saveRDS(runs, "data/all_runs_gdqtracker.rds")
  }

  runs
}
