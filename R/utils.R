#' Get the latest event
#'
#' @return `character(1)`
#' @export
#'
#' @examples
#' latest_event()
latest_event <- function() {
  gdqdonations::event_index$event[
    gdqdonations::event_index$start == max(gdqdonations::event_index$start)
  ]
}
