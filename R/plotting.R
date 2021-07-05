#' Plotting helpers
#' @param legend.position `["top"]`: See [`ggplot2::theme()`]
#' @param ... passed to [`ggplot2::theme`]
#' @export
#' @name plotting
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
#' @export
#' @rdname plotting
euro_scale <- scales::unit_format(
  suffix = "\u20ac", sep = "", big.mark = ".",
  decimal.mark = ",", accuracy = 4
)
#' @export
#' @param ... passed to [`ggplot2::dup_axis()`]
#' @rdname plotting
euro_axis <- function(...) ggplot2::dup_axis(~ . * .84, labels = euro_scale, name = NULL, ...)

p_title <- "Games Done Quick: Donation Breakdown"
p_title_r <- "Games Done Quick: Runs"
p_title_runners <- "Games Done Quick: Runners"

p_caption <- glue::glue("Donation data from gamesdonequick.com/tracker, ",
                        "run data from gdqvods.com\n",
                        "@jemus42 – gdq.tadaa-data.de")

# Setting/overriding ggplot2 components ----

#' @export
#' @rdname plotting
scale_x_year <- purrr::partial(
  ggplot2::scale_x_continuous,
  breaks = seq(0, 3e3, 1),
  minor_breaks = NULL,
  labels = ~stringr::str_replace_all(.x, "^20", "'"),
  name = ""
)

#' @export
#' @rdname plotting
scale_x_year_discrete <- purrr::partial(
  ggplot2::scale_x_discrete,
  labels = ~stringr::str_replace_all(.x, "^20", "'"),
  name = ""
)

#' @export
#' @rdname plotting
scale_y_currency <- purrr::partial(
  ggplot2::scale_y_continuous,
  labels = scales::dollar_format(),
  sec.axis = euro_axis(),
  name = ""
)

#' @export
#' @rdname plotting
scale_y_dollar <- purrr::partial(
  ggplot2::scale_y_continuous,
  labels = scales::dollar_format(),
  name = ""
)

#' @export
#' @rdname plotting
scale_x_dollar <- purrr::partial(
  ggplot2::scale_x_continuous,
  labels = scales::dollar_format(),
  name = ""
)

#' @export
#' @rdname plotting
scale_colorfill_gdq <- purrr::partial(
  ggplot2::scale_fill_manual,
  values = c("AGDQ" = "#1D3461", "SGDQ" = "#A30000"),
  aesthetics = c("color", "fill"),
  name = ""
)


# showtextdb::font_install(
#   font_desc = list(
#     showtext_name = "Cubano",
#     font_ext = "ttf",
#     regular_url = "https://dump.jemu.name/cubano-regular-webfont.ttf"
#   )
# )

# showtextdb::font_install(showtextdb::google_fonts("Fira Sans Condensed"))
