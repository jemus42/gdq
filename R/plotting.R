#' Plotting helpers
#' @param legend.position `["top"]`: See [`ggplot2::theme()`]
#' @param ... passed to [`ggplot2::theme`] or other components.
#' @export
#' @name gdq-plotting
theme_gdq <- function(legend.position = "top", ...) {
  ggplot2::theme_minimal(base_family = "Cubano") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(family = "Fira Sans Condensed"),
      legend.position = legend.position,
      panel.grid.major = ggplot2::element_line(colour = "#aaaaaa"),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(colour = "#00AEEF"),
      plot.subtitle = ggplot2::element_text(colour = "#00AEEF"),
      ...
    )
}

## Plot parts ----
#' A scale for Euros
#'
#' @export
#' @param accuracy,suffix,big.mark,decimal.mark Passed to [`scales::number_format()`].
#' @rdname gdq-plotting
#' @return A `function` as created by [`scales::number_format()`]
euro_scale <- function(
  accuracy = 4, suffix = "\u20ac", big.mark = ".",  decimal.mark = ",", ...
  ) {
  scales::number_format(accuracy = accuracy, scale = 1, prefix = "",
                suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark,
                trim = TRUE, ...)
}

scales::unit_format(
  suffix = "\u20ac", sep = "", big.mark = ".",
  decimal.mark = ",", accuracy = 4
)

#' @export
#' @param conversion_rate `[0.84]`: Manually set current dollar to euro
#' conversion rate.
#' @rdname gdq-plotting
euro_axis <- function(conversion_rate = .84) {
  ggplot2::dup_axis(~ . * conversion_rate, labels = euro_scale, name = NULL)
}

# Setting/overriding ggplot2 components ----

#' @export
#' @rdname gdq-plotting
scale_x_year <- function(...) {
  ggplot2::scale_x_continuous(
    breaks = seq(0, 3e3, 1),
    minor_breaks = NULL,
    labels = ~stringr::str_replace_all(.x, "^20", "'"),
    name = "",
    ...
  )
}
#' @export
#' @rdname gdq-plotting
scale_x_year_discrete <- function(...) {
  ggplot2::scale_x_discrete(
    labels = ~stringr::str_replace_all(.x, "^20", "'"),
    name = "",
    ...
  )
}

#' @export
#' @rdname gdq-plotting
scale_y_currency <- function(...) {
  ggplot2::scale_y_continuous(
    labels = scales::dollar_format(),
    sec.axis = euro_axis(),
    name = "",
    ...
  )
}

#' @export
#' @rdname gdq-plotting
scale_y_dollar <- function(...) {
  ggplot2::scale_y_continuous(
    labels = scales::dollar_format(),
    name = "",
    ...
  )
}

#' @export
#' @rdname gdq-plotting
scale_x_dollar <- function(...) {
  ggplot2::scale_x_continuous(
    labels = scales::dollar_format(),
    name = "",
    ...
  )
}

#' @export
#' @rdname gdq-plotting
scale_colorfill_gdq <- function(...) {
  ggplot2::scale_fill_manual(
    values = c("AGDQ" = "#1D3461", "SGDQ" = "#A30000"),
    aesthetics = c("color", "fill"),
    name = "",
    ...
  )
}
