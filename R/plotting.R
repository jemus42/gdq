#' Plotting helpers
#'
#' @param legend.position `["top"]`: See [`ggplot2::theme()`]
#' @param ... passed to [`ggplot2::theme()`] or other components.
#' @export
#' @name gdq-plotting
theme_gdq <- function(legend.position = "top", ...) {
  ggplot2::theme_minimal(
    base_size = 16,
    base_family = "Cubano"
  ) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(family = "Fira Sans Condensed"),
      axis.title = ggplot2::element_text(colour = gdq_pal[["GDQ"]]),
      legend.position = legend.position,
      legend.background = ggplot2::element_rect(colour = "transparent"),
      panel.grid.major = ggplot2::element_line(colour = "#FAFAFA"),
      panel.grid.minor = ggplot2::element_line(colour = "#FDFDFD"),
      plot.title.position = "plot",
      plot.title = ggtext::element_markdown(colour = gdq_pal[["GDQ"]]),
      plot.subtitle = ggtext::element_markdown(colour = gdq_pal[["GDQ"]]),
      plot.caption = ggtext::element_markdown(family = "Fira Sans Condensed"),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      panel.border = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent"
      ),
      ...
    )
}

# ggplot2 scales ----

#' @export
#' @rdname gdq-plotting
scale_x_year <- function(...) {
  ggplot2::scale_x_continuous(
    breaks = seq(0, 3e3, 1),
    minor_breaks = NULL,
    labels = \(x) stringr::str_replace_all(x, "^20", "'"),
    name = "",
    ...
  )
}
#' @export
#' @rdname gdq-plotting
scale_x_year_discrete <- function(...) {
  ggplot2::scale_x_discrete(
    labels = \(x) stringr::str_replace_all(x, "^20", "'"),
    name = "",
    ...
  )
}

#' @export
#' @rdname gdq-plotting
scale_y_currency <- function(...) {
  ggplot2::scale_y_continuous(
    labels = scales::dollar_format(),
    sec.axis = ggplot2::dup_axis(
      \(x) x * .usd_to_eur,
      labels = scales::unit_format(
        accuracy = 4,
        suffix = "\u20ac",
        big.mark = ".",
        decimal.mark = ","
      ),
      name = NULL
    ),
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
    values = gdq_pal,
    aesthetics = c("color", "fill"),
    name = "",
    ...
  )
}

#' GDQ Color Preset
#'
#' @export
#' @examples
#' gdq_pal
#' gdq_pal[["AGDQ"]]
gdq_pal <- c(
  "GDQ" = "#00AEEF",
  "AGDQ" = "#1D3461",
  "SGDQ" = "#A30000"
)
