#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Interval operator
#'
#' See \code{lubridate::\link[lubridate::interval]{\%--\%}} for details.
#'
#' @name %--%
#' @rdname interval
#' @keywords internal
#' @export
#' @importFrom lubridate %--%
#' @usage lhs \%--\% rhs
#' @param start,end Interval start and end points.
#' @return The result of calling `interval(start = NULL, end = NULL, tzone = tz(start))`.
NULL

