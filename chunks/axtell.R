## ---- axtell-setup -----------------------------------------------------------
pkgload::load_all(here::here(), quiet = TRUE)

## ---- axtell-load-census -----------------------------------------------------
census_env <- new.env(parent = emptyenv())
has_census <- tryCatch(
  { data("census_firm_sizes", package = "abmcourse", envir = census_env); TRUE },
  warning = function(w) FALSE,
  error   = function(e) FALSE
)
if (has_census) {
  census_sizes <- census_env$census_firm_sizes
  cat(sprintf("Census data: %d firms\n", length(census_sizes)))
} else {
  message("Census firm-size data not available. Run data-raw/census_firm_sizes.R first.")
}

## ---- axtell-fit -------------------------------------------------------------
if (has_census) {
  census_fit <- fit_power_law(census_sizes)
  cat(sprintf("Power law: alpha = %.3f,  xmin = %d,  KS = %.3f,  p = %.3f\n",
              census_fit$alpha, census_fit$xmin,
              census_fit$ks_statistic, census_fit$ks_p_value))
  cat(sprintf("Gini coefficient: %.3f\n", gini(census_sizes)))
}

## ---- axtell-plot ------------------------------------------------------------
if (has_census) {
  plot_firm_size_distribution(
    census_sizes,
    fit      = census_fit,
    title    = sprintf("US Census firm sizes (alpha = %.2f)", census_fit$alpha),
    theme_fn = theme_course_light
  )
}

## ---- axtell-plot-placeholder ------------------------------------------------
# AXTELL FIGURE PLACEHOLDER
# Replace with: plot_firm_size_distribution(axtell_sizes, fit = axtell_fit, ...)
# once Axtell's data is available (see demo/demo_axtell.R for loading code).

## ---- axtell-plot-dark -------------------------------------------------------
if (has_census) {
  plot_firm_size_distribution(
    census_sizes,
    fit      = census_fit,
    title    = sprintf("US Census firm sizes (alpha = %.2f)", census_fit$alpha),
    theme_fn = theme_course_dark
  )
}
