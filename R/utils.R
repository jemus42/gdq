#' Get the latest event
#'
#' @return `character(1)`
#' @export
#'
#' @examples
#' latest_event()
latest_event <- function() {
  event_index$event[
    event_index$start ==
      max(event_index$start, na.rm = TRUE)
  ]
}
