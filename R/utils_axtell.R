#' Plot a firm-size distribution on log-log axes with power-law fit
#'
#' Displays the complementary CDF (survival function) of firm sizes on
#' log-log axes. If a power-law fit is supplied (from `fit_power_law()`),
#' overlays the fitted line.
#'
#' @param firm_sizes Integer vector of firm sizes (one element per firm).
#' @param fit Optional list from `fit_power_law()`; if supplied, overlays
#'   the fitted line. Must contain elements `alpha` and `xmin`.
#' @param title Plot title. Default `"Firm size distribution"`.
#' @param theme_fn A ggplot2 theme function to apply. Default
#'   `theme_course_light`.
#' @return A ggplot object.
#' @export
#' @examples
#' sizes <- round(poweRlaw::rplcon(500, xmin = 1, alpha = 2.1))
#' plot_firm_size_distribution(sizes)
plot_firm_size_distribution <- function(firm_sizes, fit = NULL,
                                         title = "Firm size distribution",
                                         theme_fn = theme_course_light) {
  x    <- sort(as.integer(firm_sizes[firm_sizes >= 1L]))
  n    <- length(x)
  ccdf <- 1 - seq_len(n) / n

  df <- data.frame(x = x, ccdf = ccdf)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["ccdf"]])) +
    ggplot2::geom_point(colour = COURSE_PALETTE[["data"]], size = 0.6, alpha = 0.5) +
    ggplot2::scale_x_log10(labels = scales::label_comma()) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      x     = "Firm size (employees)",
      y     = "P(X > x)",
      title = title
    ) +
    theme_fn()

  if (!is.null(fit) && !is.null(fit$alpha) && !is.null(fit$xmin)) {
    # Power-law CCDF: P(X > x) ~ x^(-(alpha-1))
    xmin      <- fit$xmin
    alpha_hat <- fit$alpha
    x_seq     <- 10^seq(log10(xmin), log10(max(x)), length.out = 200)
    # Normalise so the line passes through (xmin, ccdf at xmin)
    ccdf_at_xmin <- mean(x >= xmin)
    y_seq     <- ccdf_at_xmin * (x_seq / xmin)^(-(alpha_hat - 1))

    line_df <- data.frame(x = x_seq, y = y_seq)
    p <- p + ggplot2::geom_line(
      data    = line_df,
      mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
      colour  = COURSE_PALETTE[["sd"]],
      linewidth = 1,
      linetype  = "dashed"
    )
  }

  p
}

#' Load Axtell firm-size data from a CSV or RDA file
#'
#' Reads firm-size data from disk. CSV files must contain a column named
#' `size`. RDA files must contain an object named `firm_sizes` or a data
#' frame with a column named `size`.
#'
#' @param path Path to file. Accepts `.csv` or `.rda` / `.RData` files.
#' @return Integer vector of firm sizes.
#' @export
#' @examples
#' \dontrun{
#' sizes <- load_firm_sizes("data/axtell_firm_sizes.csv")
#' }
load_firm_sizes <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE)
    if (!"size" %in% names(df)) {
      stop("CSV file must contain a column named 'size'")
    }
    return(as.integer(df[["size"]]))
  }

  if (ext %in% c("rda", "rdata")) {
    env <- new.env(parent = emptyenv())
    load(path, envir = env)
    objs <- ls(env)

    # Look for a vector named firm_sizes
    if ("firm_sizes" %in% objs) {
      return(as.integer(env[["firm_sizes"]]))
    }

    # Look for a data frame with a size column
    for (obj_name in objs) {
      obj <- env[[obj_name]]
      if (is.data.frame(obj) && "size" %in% names(obj)) {
        return(as.integer(obj[["size"]]))
      }
    }

    stop("RDA file must contain an object named 'firm_sizes' or a data frame with a 'size' column")
  }

  stop("Unsupported file extension: ", ext, ". Use .csv or .rda/.RData")
}
