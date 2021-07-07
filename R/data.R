#' GDQ Event Index
#'
#' @note Needs manual updating with each new event
#'
#' @examples
#' tibble::glimpse(event_index)
#'
#' event_index[nrow(event_index), c("event_name", "start", "end")]
"event_index"

#' Aggregated donation data
#'
#' All the GDQ donations;
#'
#' @source [`assemble_donations()`]
"gdq_donations"

#' Aggregated run data
#'
#' All the GDQ runs.
#'
#' @source [`assemble_runs()`]
"gdq_runs"
