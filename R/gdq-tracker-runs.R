#' Get runs from GDQ tracker
#'
#' @inheritParams get_page_count
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' get_runs(event = "sgdq2021")
#' }
get_runs <- function(event = "latest") {
  if (event == "latest") {
    event <- latest_event()
  }

  event <- toupper(event)
  url <- event_index$tracker_run_url[event_index$event == event]

  xtab <- rvest::read_html(paste0(gdq_base_url, url)) |>
    rvest::html_table()

  if (nrow(xtab[[1]]) == 0) {
    return(tibble::tibble())
  }

  xtab[[1]] |>
    purrr::set_names(c(
      "run",
      "players",
      "hosts",
      "commentators",
      "description",
      "run_start",
      "run_duration",
      "bidwars"
    )) |>
    dplyr::mutate(
      run_start = lubridate::ymd_hms(.data$run_start),
      run_duration = ifelse(run_duration == "0", "00:00:00", run_duration),
      # run_end = lubridate::ymd_hms(.data$run_end),
      run_duration = hms::as_hms(run_duration),
      #run_duration_s = as.numeric(difftime(.data$run_end, .data$run_start, units = "secs")),
      run_duration_s = as.numeric(run_duration),
      #run_duration_hms = hms::hms(seconds = .data$run_duration_s),
      event = .env$event,
      year = stringr::str_extract(.data$event, "\\d+"),
      gdq = stringr::str_remove(.data$event, "\\d+")
    ) |>
    dplyr::arrange(.data$run_start)
}

#' Summarize GDQ tracker run data by event
#'
#' @param runs Run tibble as returned by [`assemble_runs()`].
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_runs() |>
#'   summarize_runs()
#' }
summarize_runs <- function(runs) {
  runs |>
    dplyr::filter(
      # bonus games / streams
      stringr::str_detect(
        .data$run,
        "[Bb]onus ([Gg]ames|[Ss]tream)",
        negate = TRUE
      )
    ) |>
    dplyr::group_by(.data$event) |>
    dplyr::summarize(
      start_runs = min(run_start),
      end_runs = max(run_end),
      duration_d = (start_runs %--% end_runs) / lubridate::ddays(1)
    )
}

#' Update all runs from GDQ tracker
#'
#' @inheritParams assemble_donations
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_runs()
#' }
assemble_runs <- function(events = NULL, cache = FALSE) {
  if (is.null(events)) {
    events <- fs::dir_ls(
      fs::path(
        getOption("gdq_cache_dir"),
        "gamesdonequick.com"
      ),
      regexp = "runs_[as]gdq\\d+\\.rds"
    )
  }

  runs <- purrr::map_df(
    events,
    \(x) {
      readRDS(x) |>
        dplyr::mutate(
          run = as.character(.data$run),
          players = as.character(.data$players),
          description = as.character(.data$description),
          bidwars = as.character(.data$bidwars)
        )
    }
  ) |>
    dplyr::arrange(.data$run_start)

  if (cache) {
    cache_path <- fs::path(getOption("gdq_cache_dir"), "gdq_runs.rds")
    cli::cli_alert_info("Caching run data at {.file {cache_path}}")
    saveRDS(runs, cache_path)
  }

  runs
}

#' Update run data from GDQ tracker
#'
#' @inheritParams update_tracker_donations
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' update_tracker_runs(
#'   events = c("agdq2021", "sgdq2021"),
#'   ignore_cache = TRUE,
#'   in_progress = TRUE
#' )
#' }
update_tracker_runs <- function(
  events,
  ignore_cache = FALSE,
  in_progress = FALSE
) {
  prg <- cli::cli_progress_bar(name = "Getting runs", total = length(events))
  events <- toupper(events)

  purrr::walk(
    events,
    \(x) {
      cli::cli_progress_update(id = prg)
      cli::cli_text("Current event: {x}")

      out_file <- fs::path(
        getOption("gdq_cache_dir"),
        "gamesdonequick.com",
        paste0("runs_", tolower(x), ".rds")
      )

      if (!ignore_cache & file.exists(out_file)) {
        return(tibble::tibble())
      }

      if (!in_progress) {
        if (
          Sys.Date() <
            gdqdonations::event_index$end[gdqdonations::event_index$event == x]
        ) {
          return(tibble::tibble())
        }
      }

      usethis::use_directory(getOption("gdq_cache_dir"))
      get_runs(event = x) |>
        saveRDS(out_file)
    }
  )

  cli::cli_alert_success("Got runs from GDQ tracker!")
}
