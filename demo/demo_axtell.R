## demo_axtell.R
## Plots firm-size distributions and fits a power law.
## Uses Axtell's data (if available) and/or the bundled US Census firm-size data.
##
## NOTE: Axtell's data files are not included in this repository.
##       Obtain them separately from Rob Axtell and place a CSV with a column
##       named 'size' at the path assigned to AXTELL_PATH below.
##
## Run from the package root:
##   Rscript demo/demo_axtell.R

pkgload::load_all(".", quiet = TRUE)

if (!dir.exists("figures")) dir.create("figures")

AXTELL_PATH <- "data-raw/axtell_firm_sizes.csv"   # update if file is elsewhere

# ── 1. Load data ──────────────────────────────────────────────────────────────
cli::cli_h1("Firm-size data")

has_axtell <- file.exists(AXTELL_PATH)

if (!has_axtell) {
  cli::cli_alert_warning(
    "Axtell data not found at {.path {AXTELL_PATH}}. \\
     Set AXTELL_PATH to the correct location and re-run."
  )
}

# Load Census data into a local env to avoid lazy-load stub false-positives
census_env  <- new.env(parent = emptyenv())
has_census  <- tryCatch(
  {
    data("census_firm_sizes", package = "abmcourse", envir = census_env)
    TRUE
  },
  warning = function(w) FALSE,
  error   = function(e) FALSE
)

if (!has_census) {
  cli::cli_alert_warning(
    "Census firm-size data not yet built. Run data-raw/census_firm_sizes.R first."
  )
}

if (!has_axtell && !has_census) {
  cli::cli_alert_danger(
    "No firm-size data available. \\
     Please provide Axtell's CSV or build census_firm_sizes.rda."
  )
  quit(save = "no", status = 0)
}

census_firm_sizes <- if (has_census) census_env$census_firm_sizes else NULL

# ── 2. Power law fitting ──────────────────────────────────────────────────────
cli::cli_h1("Power law fitting")

plots <- list()

if (has_axtell) {
  axtell_sizes <- load_firm_sizes(AXTELL_PATH)
  cli::cli_alert_info("Axtell data: {length(axtell_sizes)} firms")

  cli::cli_progress_step("Fitting power law to Axtell data")
  axtell_fit <- fit_power_law(axtell_sizes)
  cli::cli_progress_done()

  cli::cli_alert_info(
    "Axtell: alpha = {round(axtell_fit$alpha, 3)}, \\
     xmin = {axtell_fit$xmin}, \\
     KS = {round(axtell_fit$ks_statistic, 3)}, \\
     p = {round(axtell_fit$ks_p_value, 3)}"
  )

  # Build fit line for overlay (slope = -(alpha - 1) on log-log CCDF)
  axtell_line <- list(
    slope     = -(axtell_fit$alpha - 1),
    intercept = 0   # plot_distribution will normalise automatically
  )

  plots$axtell <- plot_firm_size_distribution(
    axtell_sizes,
    fit   = axtell_fit,
    title = sprintf("Axtell firm sizes (alpha = %.2f)", axtell_fit$alpha)
  )

  ggplot2::ggsave("figures/axtell_distribution.png", plots$axtell,
                  width = 7, height = 5, dpi = 150)
}

if (has_census) {
  census_sizes <- census_firm_sizes
  cli::cli_alert_info("Census data: {length(census_sizes)} firms")

  cli::cli_progress_step("Fitting power law to Census data")
  census_fit <- fit_power_law(census_sizes)
  cli::cli_progress_done()

  cli::cli_alert_info(
    "Census: alpha = {round(census_fit$alpha, 3)}, \\
     xmin = {census_fit$xmin}, \\
     KS = {round(census_fit$ks_statistic, 3)}, \\
     p = {round(census_fit$ks_p_value, 3)}"
  )

  plots$census <- plot_firm_size_distribution(
    census_sizes,
    fit   = census_fit,
    title = sprintf("US Census firm sizes (alpha = %.2f)", census_fit$alpha)
  )

  ggplot2::ggsave("figures/census_distribution.png", plots$census,
                  width = 7, height = 5, dpi = 150)
}

# ── 3. Side-by-side if both datasets available ────────────────────────────────
if (has_axtell && has_census) {
  cmp <- plot_side_by_side(
    plots$axtell, plots$census,
    title = "Firm-size distributions: Axtell vs US Census"
  )

  # Distribution comparison
  dist_cmp <- compare_distributions(
    simulated = as.numeric(axtell_sizes),
    empirical = as.numeric(census_sizes)
  )
  cli::cli_alert_info(
    "Axtell vs Census KS: D = {round(dist_cmp$ks_statistic, 3)}, \\
     p = {round(dist_cmp$ks_p_value, 3)}"
  )
  cli::cli_alert_info(
    "Gini: Axtell = {round(dist_cmp$simulated_gini, 3)}, \\
     Census = {round(dist_cmp$empirical_gini, 3)}"
  )

  ggplot2::ggsave("figures/firmsize_comparison.png", cmp,
                  width = 12, height = 5, dpi = 150)
}

cli::cli_alert_success("Figures saved to figures/")
