#' Course colour palette
#'
#' Named character vector mapping model type to hex colour.
#' - `sd`: blue for system dynamics
#' - `abm`: plum for ABM baseline
#' - `extended`: amber for ABM extension
#' - `data`: green for empirical data
#'
#' @export
COURSE_PALETTE <- c(
  sd       = "#2E86AB",   # blue  -- system dynamics
  abm      = "#A23B72",   # plum  -- ABM baseline
  extended = "#F18F01",   # amber -- ABM extension
  data     = "#4B8B3B"    # green -- empirical data
)

#' Slide background colour (dark navy)
#' @export
SLIDE_BG     <- "#1B2A4A"

#' Slide accent colour (amber, used for frame titles and block borders)
#' @export
SLIDE_ACCENT <- "#F18F01"

#' Slide body text colour (off-white)
#' @export
SLIDE_TEXT   <- "#E8E8E8"

#' Slide code-panel background (slightly lighter navy)
#' @export
SLIDE_PANEL  <- "#1E3A5F"

#' Light ggplot2 theme for the course document
#'
#' Based on `theme_minimal()`. White panel background, dark axis text,
#' light gray grid lines. Used in all course document chunks.
#'
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + theme_course_light()
theme_course_light <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = "grey88"),
      panel.grid.minor  = ggplot2::element_line(colour = "grey94"),
      axis.text         = ggplot2::element_text(colour = "grey30"),
      axis.title        = ggplot2::element_text(colour = "grey20"),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.text       = ggplot2::element_text(colour = "grey20"),
      legend.title      = ggplot2::element_text(colour = "grey20"),
      strip.text        = ggplot2::element_text(colour = "grey20"),
      plot.title        = ggplot2::element_text(colour = "grey10", face = "bold"),
      legend.position   = "bottom"
    )
}

#' Dark ggplot2 theme for the beamer slides
#'
#' Panel background uses `SLIDE_BG`. Grid lines are slightly lighter navy.
#' Axis text, legend text, and strip text use `SLIDE_TEXT`.
#' Used in all slide chunks. Produces figures legible on a projected dark
#' background.
#'
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + theme_course_dark()
theme_course_dark <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = SLIDE_BG,    colour = NA),
      plot.background   = ggplot2::element_rect(fill = SLIDE_BG,    colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = "#243a5e"),
      panel.grid.minor  = ggplot2::element_line(colour = "#1e3050"),
      axis.text         = ggplot2::element_text(colour = SLIDE_TEXT),
      axis.title        = ggplot2::element_text(colour = SLIDE_TEXT),
      legend.background = ggplot2::element_rect(fill = SLIDE_BG,    colour = NA),
      legend.text       = ggplot2::element_text(colour = SLIDE_TEXT),
      legend.title      = ggplot2::element_text(colour = SLIDE_TEXT),
      strip.text        = ggplot2::element_text(colour = SLIDE_TEXT),
      plot.title        = ggplot2::element_text(colour = SLIDE_TEXT, face = "bold"),
      legend.position   = "bottom"
    )
}

#' Plot time series from a model results tibble
#'
#' @param results A long-format tibble with columns: `time` (or `step`),
#'   `variable`, `value`.
#' @param title Plot title string.
#' @param colour_map Named character vector mapping variable names to hex
#'   colours. Defaults to `COURSE_PALETTE`.
#' @param log_y Logical; use log scale on y-axis. Default `FALSE`.
#' @param theme_fn A ggplot2 theme function to apply. Default
#'   `theme_course_light`.
#' @return A ggplot object.
#' @export
#' @examples
#' results <- tibble::tibble(
#'   time     = rep(1:10, 2),
#'   variable = rep(c("prey", "predators"), each = 10),
#'   value    = c(runif(10, 30, 50), runif(10, 5, 15))
#' )
#' plot_time_series(results, title = "Example")
plot_time_series <- function(results, title, colour_map = NULL,
                             log_y = FALSE, theme_fn = theme_course_light) {
  # Detect the time column (either "time" or "step")
  if ("time" %in% names(results)) {
    time_col <- "time"
  } else if ("step" %in% names(results)) {
    time_col <- "step"
  } else {
    stop("results must contain a column named 'time' or 'step'")
  }

  if (is.null(colour_map)) {
    colour_map <- COURSE_PALETTE
  }

  p <- ggplot2::ggplot(
    results,
    ggplot2::aes(
      x     = .data[[time_col]],
      y     = .data[["value"]],
      colour = .data[["variable"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(x = time_col, y = "value", title = title, colour = NULL) +
    theme_fn()

  # Apply colour scale only when all variables are named in colour_map
  vars <- unique(results[["variable"]])
  if (all(vars %in% names(colour_map))) {
    p <- p + ggplot2::scale_colour_manual(values = colour_map[vars])
  }

  if (log_y) {
    p <- p + ggplot2::scale_y_log10()
  }

  p
}

#' Plot a phase portrait (x vs y trajectory)
#'
#' @param results Tibble with columns `x` and `y` (e.g., prey and predators).
#' @param xlabel Axis label for the x-axis.
#' @param ylabel Axis label for the y-axis.
#' @param title Plot title.
#' @param theme_fn A ggplot2 theme function to apply. Default
#'   `theme_course_light`.
#' @return A ggplot object.
#' @export
#' @examples
#' results <- tibble::tibble(x = cos(seq(0, 2 * pi, length.out = 100)),
#'                           y = sin(seq(0, 2 * pi, length.out = 100)))
#' plot_phase_portrait(results, xlabel = "Prey", ylabel = "Predators",
#'                     title = "Phase portrait")
plot_phase_portrait <- function(results, xlabel, ylabel, title,
                                theme_fn = theme_course_light) {
  ggplot2::ggplot(results, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
    ggplot2::geom_path(colour = COURSE_PALETTE[["sd"]], linewidth = 0.8,
                       alpha = 0.85) +
    ggplot2::geom_point(data = results[1, ], colour = COURSE_PALETTE[["abm"]],
                        size = 3) +
    ggplot2::labs(x = xlabel, y = ylabel, title = title) +
    theme_fn()
}

#' Plot an empirical distribution, optionally on log-log axes
#'
#' @param values Numeric vector of observed values.
#' @param title Plot title.
#' @param log_log Logical; plot on log-log axes. Default `FALSE`.
#' @param fit Optional list with elements `slope` and `intercept` for a
#'   reference power-law line (on log-log scale).
#' @param theme_fn A ggplot2 theme function to apply. Default
#'   `theme_course_light`.
#' @return A ggplot object.
#' @export
#' @examples
#' plot_distribution(rexp(200), title = "Exponential distribution")
plot_distribution <- function(values, title, log_log = FALSE, fit = NULL,
                              theme_fn = theme_course_light) {
  df <- data.frame(x = sort(values))

  if (log_log) {
    # Complementary CDF (survival function) on log-log axes
    n   <- length(df$x)
    df  <- data.frame(
      x    = df$x,
      ecdf = seq_len(n) / n
    )
    df$ccdf <- 1 - df$ecdf

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]],
                                           y = .data[["ccdf"]])) +
      ggplot2::geom_point(colour = COURSE_PALETTE[["data"]], size = 0.8,
                          alpha = 0.6) +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10() +
      ggplot2::labs(x = "value", y = "P(X > x)", title = title) +
      theme_fn()

    if (!is.null(fit) && !is.null(fit$slope) && !is.null(fit$intercept)) {
      # Overlay a reference power-law line
      x_range <- range(log10(df$x[df$x > 0]), na.rm = TRUE)
      line_df  <- data.frame(
        x = 10^seq(x_range[1], x_range[2], length.out = 100)
      )
      line_df$y <- 10^(fit$intercept + fit$slope * log10(line_df$x))
      p <- p + ggplot2::geom_line(
        data    = line_df,
        mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
        colour  = COURSE_PALETTE[["sd"]],
        linewidth = 1,
        linetype  = "dashed"
      )
    }
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]])) +
      ggplot2::geom_histogram(fill = COURSE_PALETTE[["data"]], colour = "white",
                              bins = 30, alpha = 0.8) +
      ggplot2::labs(x = "value", y = "count", title = title) +
      theme_fn()
  }

  p
}

#' Combine two ggplot objects side by side using patchwork
#'
#' @param left A ggplot object for the left panel.
#' @param right A ggplot object for the right panel.
#' @param title Overall title string (patchwork annotation). Default `NULL`.
#' @param theme_fn A ggplot2 theme function used for the annotation title.
#'   Default `theme_course_light`.
#' @return A patchwork object.
#' @export
#' @examples
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p2 <- ggplot(mtcars, aes(hp, wt))  + geom_point()
#' plot_side_by_side(p1, p2, title = "Comparison")
plot_side_by_side <- function(left, right, title = NULL,
                              theme_fn = theme_course_light) {
  combined <- left + right
  if (!is.null(title)) {
    combined <- combined +
      patchwork::plot_annotation(
        title = title,
        theme = theme_fn()
      )
  }
  combined
}
